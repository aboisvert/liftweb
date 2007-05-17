package net.liftweb.http

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0

\*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{Map, HashMap, ArrayBuffer}
import scala.xml.{NodeSeq, Elem, Text, UnprefixedAttribute, Null, MetaData}
import scala.collection.immutable.{ListMap}
import net.liftweb.util.{Helpers, ThreadGlobal}
import net.liftweb.mapper.{Safe, ValidationIssue}
import Helpers._

/**
 * An object representing the current state of the HTTP request and response
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around
 */
object S {
  /**
   * The current session
   */
  private val _request = new ThreadGlobal[RequestState];
  private val _servletRequest = new ThreadGlobal[HttpServletRequest];
  private val functionMap = new ThreadGlobal[HashMap[String, AFuncHolder]];
  private val _notice = new ThreadGlobal[ArrayBuffer[(NoticeType.Value, NodeSeq)]];
  private val _oldNotice = new ThreadGlobal[Seq[(NoticeType.Value, NodeSeq)]];
  private val inS = {
    val ret = new ThreadGlobal[boolean];
    ret := false
    ret           
  }
  private val snippetMap = new ThreadGlobal[HashMap[String, NodeSeq => NodeSeq]]
  private val _reqVar = new ThreadGlobal[HashMap[String, String]]
  
  /**
   * Get the current HttpServletSession
   *
   * @return the current session
   */
  def request = {_request.value}
  
  /**
   * Test the current request to see if it's a POST
   */
  def post_? = request.post_?

  /**
   * Test the current request to see if it's a POST
   */
  def get_? = request.get_?
  
  def redirectTo[T](where: String): T = {throw new RedirectException("Not Found", where)}
  
  private val executionInfo = new ThreadGlobal[HashMap[String, Function[Array[String], Any]]]
  
  private val currCnt = new ThreadGlobal[int]
  
  def nc : String = {
    val n = currCnt.value
    currCnt := n + 1
    String.format("%06d", Array(new Integer(n)))
  }
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ArrayBuffer[(NoticeType.Value, NodeSeq)])(f)
  }
  
  /**
   * Initialize the current "State" session
   */
  def init[B](request : RequestState)(f : => B) : B = {
    _oldNotice.doWith(Nil) {
      _init(request)(f)
    }

  }
  
  private def _init[B](request: RequestState)(f : => B): B = {
    _reqVar.doWith(new HashMap) {
    snippetMap.doWith(new HashMap) {
      inS.doWith(true) {
          initNotice {
            functionMap.doWith(new HashMap[String, AFuncHolder]) {
              this._request.doWith(request) {
                this.currCnt.doWith(0)(f)
              }
            }
          }
        }
      }
    }
  }
  
  object vars {
    def apply(what: String): Option[String] = S._reqVar.value.get(what)
    def update(what: String, value: String) = S._reqVar.value(what) = value
  }
  
  def setVars[T](attr: MetaData)(f: => T): T = {
    val old = _reqVar.value
    val ht: HashMap[String, String] = new HashMap
    old.elements.foreach(v => ht(v._1) = v._2)
    attr.elements.foreach(e => ht(e.key) = e.value.text)
    _reqVar.doWith(ht)(f)
  }
  
  def initIfUninitted[B](f: => B) : B = {
    if (inS.value) f
    else init(RequestState.nil)(f)
  }
  
  def init[B](request: RequestState, servletRequest: HttpServletRequest, oldNotices: Seq[(NoticeType.Value, NodeSeq)])(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
      this._servletRequest.doWith(servletRequest) {
        _init(request)(f)
      }
    }
  }
  
  def get(what: String): Option[String] = {
    _servletRequest.value match {
      case null => None
      case s => s.getSession.getAttribute(what) match {
	case null => None
	case n => {
	  if (n.isInstanceOf[String]) Some(n.asInstanceOf[String])
	  else None
	}
      }
    }
  }
  
  // def invokeSnippet[B](snippetName: String)(f: => B):B = _invokedAs.doWith(snippetName)(f)
  def invokedAs: String = _reqVar.value.get("type") getOrElse ""
  
  def set(name: String, value: String) {
    if (_servletRequest.value ne null) {
      _servletRequest.value.getSession.setAttribute(name, value)
    }
  }
  
  def unset(name: String) {
    if (_servletRequest.value ne null) {
      _servletRequest.value.getSession.removeAttribute(name)
    }
  }
  
  def servletRequest = _servletRequest.value
  
  def getFunctionMap: Map[String, AFuncHolder] = {
    functionMap.value match {
      case null => Map.empty
      case s => s
    }
  }
  
  def locateSnippet(name: String): Option[NodeSeq => NodeSeq] = snippetMap.value.get(name)
  
  def mapSnippet(name: String, func: NodeSeq => NodeSeq) {snippetMap.value(name) = func}

 
  /*
  def fL(in: List[String] => boolean): String = {
    val name = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(name, in)
    name
  }
  
  def fL(name: String, inf: List[String] => boolean): String = {
    addFunctionMap(name, inf)
    name
  }*/
  
  def addFunctionMap(name: String, value: AFuncHolder) {
    functionMap.value += (name -> value)
  }
  def mapFunction(name: String, f: AFuncHolder): String = {
    val ret = ""+nc+"_"+name+"_"+randomString(5)
    functionMap.value += (ret -> f)
    ret
  }

  private def booster(lst: List[String], func: String => Any):boolean  = {
    lst.foreach(v => func(v))
    true
  }
  
  sealed abstract class FormElementPieces
  case class Id(name: String) extends FormElementPieces
  case class Cls(name: String) extends FormElementPieces
  case class Val(value: String) extends FormElementPieces
  
  private def wrapFormElement(in: Elem, params: List[FormElementPieces]): Elem = {
    params match {
      case Nil => in
      case Id(name) :: rs => wrapFormElement(in % new UnprefixedAttribute("id", name, Null), rs)
      case Cls(name) :: rs => wrapFormElement(in % new UnprefixedAttribute("class", name, Null), rs)
      case Val(name) :: rs => wrapFormElement(in % new UnprefixedAttribute("value", name, Null), rs)
    }
  }
  
  private def makeFormElement(name: String, func: AFuncHolder, params: Seq[FormElementPieces]): NodeSeq =
    wrapFormElement(<input type={name} name={f(func)}/>, params.toList)
 
  /**
    * create an anchor tag around a body which will do an AJAX call and invoke the function
    *
    * @param func - the function to invoke when the link is clicked
    * @param body - the NodeSeq to wrap in the anchor tag
    */
  def a(func: () => Any, body: NodeSeq): NodeSeq = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(key, (a: List[String]) => {func(); true})
    <lift:a key={key}>{body}</lift:a>
  }

    
    /**
       * create an anchor tag around a body 
       *
       * @param func - the function to invoke when the link is clicked
       * @param body - the NodeSeq to wrap in the anchor tag
       */
     def link(to: String, func: () => Any, body: NodeSeq): NodeSeq = {
       val key = "F"+System.nanoTime+"_"+randomString(3)
       addFunctionMap(key, (a: List[String]) => {func(); true})
       <a href={to+"?"+key+"=_"}>{body}</a>
     }
    
    def text_*(func: AFuncHolder, params: FormElementPieces*): NodeSeq = makeFormElement("text", func, params)
    def password_*(func: AFuncHolder, params: FormElementPieces*): NodeSeq = makeFormElement("password", func, params)
    def hidden_*(func: AFuncHolder, params: FormElementPieces*): NodeSeq = makeFormElement("hidden", func, params)
    def submit_*(func: AFuncHolder, params: FormElementPieces*): NodeSeq = makeFormElement("submit", func, params)
  def text(func: String => Any, params: FormElementPieces*): NodeSeq = makeFormElement("text", SFuncHolder(func), params)
  def password(func: String => Any, params: FormElementPieces*): NodeSeq = makeFormElement("password", SFuncHolder(func), params)
  def hidden(func: String => Any, params: FormElementPieces*): NodeSeq = makeFormElement("hidden", SFuncHolder(func), params)
  def submit(func: String => Any, params: FormElementPieces*): NodeSeq = makeFormElement("submit", SFuncHolder(func), params)
  def select(opts: List[(String, String)], deflt: Option[String], func: String => Any, params: FormElementPieces*): NodeSeq = 
    select_*(opts, deflt, SFuncHolder(func), params :_*)
    
    // FIXME wrap the select in a filter to insure that stuff received is in the set of things sent
  def select_*(opts: List[(String, String)],deflt: Option[String], func: AFuncHolder, params: FormElementPieces*): NodeSeq =  
    wrapFormElement(<select name={f(func)}>{
      opts.flatMap(o => <option value={o._1}>{o._2}</option> % (if (deflt.filter(_ == o._1).isDefined) new UnprefixedAttribute("selected", "true", Null) else Null))
    }</select>, params.toList)
    
    
     def multiSelect(opts: List[(String, String)], deflt: List[String], func: String => Any, params: FormElementPieces*): NodeSeq = 
       multiSelect_*(opts, deflt, SFuncHolder(func), params :_*)

     def multiSelect_*(opts: List[(String, String)], deflt: List[String],func: AFuncHolder, params: FormElementPieces*): NodeSeq =  
    wrapFormElement(<select multiple="true" name={f(func)}>{
      opts.flatMap(o => <option value={o._1}>{o._2}</option> % (if (deflt.contains(o._1)) new UnprefixedAttribute("selected", "true", Null) else Null))
    }</select>, params.toList)
    

    def textarea(value: String, func: String => Any, params: FormElementPieces*): NodeSeq = textarea_*(value, SFuncHolder(func), params :_*)
    
    def textarea_*(value: String, func: AFuncHolder, params: FormElementPieces*): NodeSeq = 
      wrapFormElement(<textarea name={f(func)}>{value}</textarea>, params.toList) 
    
    def radio(opts: List[String], deflt: Option[String], func: String => Any, params: FormElementPieces*): RadioHolder =
      radio_*(opts, deflt, SFuncHolder(func), params :_*)
      
    def radio_*(opts: List[String], deflt: Option[String], func: AFuncHolder, params: FormElementPieces*): RadioHolder = {
      val name = f(func)
      val itemList = opts.map(v => RadioItem(v, wrapFormElement(<input type="radio" name={name} value={v}/> % 
          (if (deflt.filter(_ == v).isDefined) new UnprefixedAttribute("checked", "checked", Null) else Null), params.toList)))
      RadioHolder(itemList)
    }
    
  case class RadioItem(key: String, xhtml: NodeSeq)
    
  case class RadioHolder(items: List[RadioItem]) {
      def apply(in: String) = items.filter(_.key == in).head.xhtml
      def apply(in: int) = items(in).xhtml
    }
  
  def checkbox(value: boolean, func: boolean => Any, params: FormElementPieces*): NodeSeq = {
    def from(f: boolean => Any): List[String] => boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)), params :_*)
  }
    
  def checkbox_*(value: boolean, func: AFuncHolder, params: FormElementPieces*): NodeSeq = {
    val name = f(func)
    <input type="hidden" name={name} value="false"/> ++
      wrapFormElement(<input type="checkbox" name={name} checked={value.toString}/>, params.toList)
  }
    
    /*
    
    private def makeFormElement(name: String, func: AFuncHolder, params: Seq[FormElementPieces]): NodeSeq =
      wrapFormElement(<input type={name} name={f(func)}/>, params.toList)
*/
  /*
  private def makeFormElementL(name: String, func: List[String] => boolean, params: Seq[FormElementPieces]): NodeSeq =
    wrapFormElement(<input type={name} name={fL(func)}/>, params.toList)
  */
    
  /*
  def textL(func: List[String] => boolean, params: FormElementPieces*): NodeSeq = makeFormElementL("text", func, params)
  def passwordL(func: List[String] => boolean, params: FormElementPieces*): NodeSeq = makeFormElementL("password", func, params)
  def hiddenL(func: List[String] => boolean, params: FormElementPieces*): NodeSeq = makeFormElementL("hidden", func, params)
  def submitL(func: List[String] => boolean, params: FormElementPieces*): NodeSeq = makeFormElementL("submit", func, params)
  */
  
  // implicit def toSFunc(in: String => Any): AFuncHolder = SFuncHolder(in)
  implicit def toLFunc(in: List[String] => boolean): AFuncHolder = LFuncHolder(in)
  implicit def toNFunc(in: () => Any): AFuncHolder = NFuncHolder(in)
  
  abstract class AFuncHolder {
    def apply(in: List[String]): boolean
  }
  case class SFuncHolder(func: String => Any) extends AFuncHolder {
    def apply(in: List[String]): boolean = {in.foreach(func(_)); true}
  }
  case class LFuncHolder(func: List[String] => boolean) extends AFuncHolder  {
    def apply(in: List[String]): boolean = func(in)
  }
  case class NFuncHolder(func: () => Any) extends AFuncHolder  {
    def apply(in: List[String]): boolean = {in.foreach(s => func()); true}
  }
  
  def f(in: AFuncHolder): String = {
    f("F"+System.nanoTime+"_"+randomString(3), in)
  }
  
  def f(name: String, inf: AFuncHolder): String = {
    addFunctionMap(name, inf)
    name
  }
  
  
  def params(n: String) = request.params(n)
  def param(n: String): Option[String] = request.param(n)
  
  def error(n: String) {error(Text(n))}
  def error(n: NodeSeq) {_notice.value += (NoticeType.Error, n)}
  def notice(n: String) {notice(Text(n))}
  def notice(n: NodeSeq) {_notice.value += (NoticeType.Notice, n)}
  def warning(n: String) {warning(Text(n))}
  def warning(n: NodeSeq) {_notice.value += (NoticeType.Warning, n)}
  
  def error(vi: List[ValidationIssue]) {_notice.value ++= vi.map{i => (NoticeType.Error, <span><b>{i.field.name}</b>: {i.msg}</span>)}}
  
  def getNotices = _notice.value.toList
  
  def errors: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Error).map(_._2))
  def notices: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Notice).map(_._2))
  def warnings: Seq[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Warning).map(_._2))
  def clearCurrentNotices {_notice.value.clear}
  
}

object NoticeType extends Enumeration {
  val Notice, Warning, Error = Value
}
