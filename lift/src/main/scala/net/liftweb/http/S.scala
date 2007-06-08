package net.liftweb.http

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0

\*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{HashMap, ArrayBuffer}
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
    val ret = new ThreadGlobal[Boolean];
    ret := false
    ret           
  }
  private val snippetMap = new ThreadGlobal[HashMap[String, NodeSeq => NodeSeq]]
  private val _attrs = new ThreadGlobal[HashMap[String, String]]
  private val _stateInfo = new ThreadGlobal[VarStateHolder]
  
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
  
  private val currCnt = new ThreadGlobal[Int]
  
  def nc : String = {
    val n = currCnt.value
    currCnt := n + 1
    String.format("%06d", Array(new Integer(n)))
  }
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ArrayBuffer[(NoticeType.Value, NodeSeq)])(f)
  }
  
  /**
   * Initialize the current request session
   */
  def init[B](request : RequestState, vsh: VarStateHolder)(f : => B) : B = {
    _oldNotice.doWith(Nil) {
      _init(request, vsh)(f)
    }

  }
  
  private def _init[B](request: RequestState, vsh: VarStateHolder)(f : => B): B = {
    _stateInfo.doWith(vsh) {
    _attrs.doWith(new HashMap) {
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
  }
  
  object state {
    def apply(name: String): Option[String] = _stateInfo.value match {
      case null => None
      case v => v(name)
    }
    def update(name: String, value: String) {_stateInfo.value match
      {
      case null => 
      case v => v(name) = value
      }
    }
    def -=(name: String) {
      _stateInfo.value match {
        case null =>
        case v => v -= name
      }
    }
  }
  
  object attr {
    def apply(what: String): Option[String] = S._attrs.value.get(what)
    def update(what: String, value: String) = S._attrs.value(what) = value
  }
  
  def setVars[T](attr: MetaData)(f: => T): T = {
    val old = _attrs.value
    val ht: HashMap[String, String] = new HashMap
    old.elements.foreach(v => ht(v._1) = v._2)
    attr.elements.foreach(e => ht(e.key) = e.value.text)
    _attrs.doWith(ht)(f)
  }
  
  def initIfUninitted[B](vsh: VarStateHolder)(f: => B) : B = {
    if (inS.value) f
    else init(RequestState.nil, vsh)(f)
  }
  
  def init[B](request: RequestState, servletRequest: HttpServletRequest, oldNotices: Seq[(NoticeType.Value, NodeSeq)], vsh: VarStateHolder)(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
      this._servletRequest.doWith(servletRequest) {
        _init(request, vsh)(f)
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
  def invokedAs: String = _attrs.value.get("type") getOrElse ""
  
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
  
  def getFunctionMap: scala.collection.Map[String, AFuncHolder] = {
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

  private def booster(lst: List[String], func: String => Any): Boolean  = {
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
  
  // List[value, display]
  def select(opts: List[(String, String)], deflt: Option[String], func: String => Any, params: FormElementPieces*): NodeSeq = 
    select_*(opts, deflt, SFuncHolder(func), params :_*)
    
    // FIXME wrap the select in a filter to insure that stuff received is in the set of things sent
  def select_*(opts: List[(String, String)],deflt: Option[String], func: AFuncHolder, params: FormElementPieces*): NodeSeq =  
    wrapFormElement(<select name={f(func)}>{
      opts.flatMap(o => <option value={o._1}>{o._2}</option> % selected(deflt.filter(_ == o._1).isDefined))
    }</select>, params.toList)
    
    
    private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "true", Null) else Null
    
     def multiSelect(opts: List[(String, String)], deflt: List[String], func: String => Any, params: FormElementPieces*): NodeSeq = 
       multiSelect_*(opts, deflt, SFuncHolder(func), params :_*)

     def multiSelect_*(opts: List[(String, String)], deflt: List[String],func: AFuncHolder, params: FormElementPieces*): NodeSeq =  
    wrapFormElement(<select multiple="true" name={f(func)}>{
      opts.flatMap(o => <option value={o._1}>{o._2}</option> % selected(deflt.contains(o._1)))
    }</select>, params.toList)
    

    def textarea(value: String, func: String => Any, params: FormElementPieces*): NodeSeq = textarea_*(value, SFuncHolder(func), params :_*)
    
    def textarea_*(value: String, func: AFuncHolder, params: FormElementPieces*): NodeSeq = 
      wrapFormElement(<textarea name={f(func)}>{value}</textarea>, params.toList) 
    
    def radio(opts: List[String], deflt: Option[String], func: String => Any, params: FormElementPieces*): ChoiceHolder[String] =
      radio_*(opts, deflt, SFuncHolder(func), params :_*)
      
    def radio_*(opts: List[String], deflt: Option[String], func: AFuncHolder, params: FormElementPieces*): ChoiceHolder[String] = {
      val name = f(func)
      val itemList = opts.map(v => ChoiceItem(v, wrapFormElement(<input type="radio" name={name} value={v}/> % 
        checked(deflt.filter(_ == v).isDefined), params.toList)))
      ChoiceHolder(itemList)
    }
    
  case class ChoiceItem[T](key: T, xhtml: NodeSeq)
    
  case class ChoiceHolder[T](items: List[ChoiceItem[T]]) {
      def apply(in: T) = items.filter(_.key == in).head.xhtml
      def apply(in: Int) = items(in).xhtml
      def map[A](f: ChoiceItem[T] => A) = items.map(f)
      def flatMap[A](f: ChoiceItem[T] => Iterable[A]) = items.flatMap(f)
      def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)
      def toForm: NodeSeq = flatMap(c => <span>{c.xhtml}&nbsp;{c.key.toString}<br /></span>)
    }
  
  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null 
  
  def checkbox[T](possible: List[T], actual: List[T], func: List[T] => Any, params: FormElementPieces*): ChoiceHolder[T] = {
    val len = possible.length
    val name = f(LFuncHolder( (strl: List[String]) => {func(strl.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true}))
    val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    
    ChoiceHolder(possible.zipWithIndex.map(p => 
      ChoiceItem(p._1, wrapFormElement(<input type="checkbox" name={name} value={p._2.toString}/> % checked(actual.contains(p._1)),
          realParams) ++ (if (p._2 == 0) <input type="hidden" name={name} value="-1"/> else Nil))))
  }
  
  def checkbox(value: Boolean, func: Boolean => Any, params: FormElementPieces*): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)), params :_*)
  }
    
  def checkbox_*(value: Boolean, func: AFuncHolder, params: FormElementPieces*): NodeSeq = {
    val name = f(func)
    val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    <input type="hidden" name={name} value="false"/> ++
      wrapFormElement(<input type="checkbox" name={name} value="true" /> % checked(value), realParams)
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
  implicit def toLFunc(in: List[String] => Boolean): AFuncHolder = LFuncHolder(in)
  implicit def toNFunc(in: () => Any): AFuncHolder = NFuncHolder(in)
  
  abstract class AFuncHolder {
    def apply(in: List[String]): Boolean
  }
  case class SFuncHolder(func: String => Any) extends AFuncHolder {
    def apply(in: List[String]): Boolean = {in.foreach(func(_)); true}
  }
  case class LFuncHolder(func: List[String] => Boolean) extends AFuncHolder  {
    def apply(in: List[String]): Boolean = func(in)
  }
  case class NFuncHolder(func: () => Any) extends AFuncHolder  {
    def apply(in: List[String]): Boolean = {in.foreach(s => func()); true}
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

class VarStateHolder(val session: Session, initVars: Map[String, String],setter: Option[Map[String, String] => Any], val local: Boolean) {
  //def this(s: Session) = this(s, s.currentVars, false)
  
  private var vars = initVars
  
  def apply(name: String): Option[String] = vars.get(name)
  def -=(name: String) {
    vars = vars - name
    setter.foreach(_(vars))
    if (local) session.stateVar - name
    else session ! UpdateState(name, None)
  }
  def update(name: String, value: String) {
    vars = vars + name -> value
    setter.foreach(_(vars))
    if (local) session.stateVar(name) = value
    else session ! UpdateState(name, Some(value))
  }
  
}
