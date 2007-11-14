package net.liftweb.http

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0

\*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.xml.{NodeSeq, Elem, Text, UnprefixedAttribute, Null, MetaData}
import scala.collection.immutable.{ListMap}
import net.liftweb.util.{Helpers, ThreadGlobal, LoanWrapper, Can, Empty, Full, Failure}
import net.liftweb.mapper.{Safe, ValidationIssue, MegaProtoUser}
import Helpers._
import js._
import java.io.InputStream
import java.util.Locale

/**
 * An object representing the current state of the HTTP request and response
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around
 */
object S {
  case class RewriteHolder(name: String, rewrite: LiftServlet.RewritePf)
  case class DispatchHolder(name: String, dispatch: LiftServlet.DispatchPf)
  case class TemplateHolder(name: String, template: LiftServlet.TemplatePf)
  
  /**
   * The current session
   */
  private val _request = new ThreadGlobal[RequestState]
  private val _servletRequest = new ThreadGlobal[HttpServletRequest]
  private val _functionMap = new ThreadGlobal[HashMap[String, AFuncHolder]]
  private val _notice = new ThreadGlobal[ListBuffer[(NoticeType.Value, NodeSeq)]]
  private val _oldNotice = new ThreadGlobal[Seq[(NoticeType.Value, NodeSeq)]];
  private val inS = (new ThreadGlobal[Boolean]).set(false)
  private val snippetMap = new ThreadGlobal[HashMap[String, NodeSeq => NodeSeq]]
  private val _attrs = new ThreadGlobal[HashMap[String, String]]
  private val _stateInfo = new ThreadGlobal[VarStateHolder]
  private val _requestVar = new ThreadGlobal[HashMap[String, Any]]
  private val _sessionInfo = new ThreadGlobal[LiftSession]
  private val _queryLog = new ThreadGlobal[ListBuffer[(String, Long)]]

  /**
   * Get the current HttpServletSession
   *
   * @return the current session
   */
  def request = {_request.value}
  
  def sessionTemplater: List[TemplateHolder] = _servletRequest.value match {
    case null => Nil
    case r => r.getSession.getAttribute(LiftServlet.SessionTemplateTableName) match {
      case rw: List[TemplateHolder] => rw
      case _ => Nil
    }
  }
  
  def action: String = request match {
    case null => ""
    case r => r.uri
  }
  
  def sessionRewriter: List[RewriteHolder] = _servletRequest.value match {
    case null => Nil
    case r => r.getSession.getAttribute(LiftServlet.SessionRewriteTableName) match {
      case rw: List[RewriteHolder] => rw
      case _ => Nil
    }
  }
  def sessionDispatcher: List[DispatchHolder] = _servletRequest.value match {
    case null => Nil
    case r => r.getSession.getAttribute(LiftServlet.SessionDispatchTableName) match {
      case rw: List[DispatchHolder] => rw
      case _ => Nil
    }
  }

  /**
   * Returns the Locale for this request based on the HTTP request's 
   * Accept-Language header. If that header corresponds to a Locale
   * that's installed on this JVM then return it, otherwise return the
   * default Locale for this JVM.
   */
  def locale: Locale = request match {
    case null => Locale.getDefault()
    case r => {
      r.request.getLocale() match {
	case l : Locale => l
	case _ => Locale.getDefault()
      }
    }
  }
  
  /**
   * Return this User's Locale if they have one, otherwise return
   * the HTTP request's Locale if available.
   */
  def locale[T <: net.liftweb.mapper.MegaProtoUser[T]](user : MegaProtoUser[T]): Locale = {
    user.locale.isAsLocale match {
      case l: Locale => l
      case _ => locale
    }
  }


  private def reduxio(in: List[LiftServlet.DispatchPf]): LiftServlet.DispatchPf = in match {
    case Nil => Map.empty
    case x :: Nil => x
    case x :: xs => x orElse reduxio(xs)
  }
  def highLevelSessionDispatcher: LiftServlet.DispatchPf = reduxio(highLevelSessionDispatchList.map(_.dispatch))
  def highLevelSessionDispatchList: List[DispatchHolder] = _servletRequest.value match {
  case null => Nil
  case r => r.getSession.getAttribute(HighLevelSessionDispatchTableName) match {
    case null => Nil
    case li: List[DispatchHolder] => li
    case _ => Nil
  }
  }    
  
  val HighLevelSessionDispatchTableName = "$lift$__HighLelelDispatchTable__"
  def addHighLevelSessionDispatcher(name: String, disp: LiftServlet.DispatchPf) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(HighLevelSessionDispatchTableName, DispatchHolder(name, disp) :: highLevelSessionDispatchList.filter(_.name != name))
    }    
  }
  
  def removeHighLevelSessionDispatcher(name: String) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(HighLevelSessionDispatchTableName, highLevelSessionDispatchList.filter(_.name != name))
    }    
  }
  
  def addSessionTemplater(name: String, rw: LiftServlet.TemplatePf) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(LiftServlet.SessionTemplateTableName, TemplateHolder(name, rw) :: sessionTemplater.filter(_.name != name))
    }
  }
  
  def addSessionRewriter(name: String, rw: LiftServlet.RewritePf) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(LiftServlet.SessionRewriteTableName, RewriteHolder(name, rw) :: sessionRewriter.filter(_.name != name))
    }
  }

  def removeSessionRewriter(name: String) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(LiftServlet.SessionRewriteTableName, sessionRewriter.remove(_.name == name))
    }
  }
  
  def removeSessionTemplater(name: String) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(LiftServlet.SessionTemplateTableName, sessionTemplater.remove(_.name == name))
    }
  }  

  def addSessionDispatcher(name: String, rw: LiftServlet.DispatchPf) {
    _servletRequest.value match {
    case null => 
    case r => val newDisp = DispatchHolder(name, rw) :: sessionDispatcher.filter(_.name != name)
        r.getSession.setAttribute(LiftServlet.SessionDispatchTableName, newDisp)
    }
  }

  def removeSessionDispatcher(name: String) {
    _servletRequest.value match {
    case null => 
    case r => r.getSession.setAttribute(LiftServlet.SessionDispatchTableName, sessionDispatcher.remove(_.name == name))
    }
  }

  
  
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
  
  /**
    * A string with the "next count"
    */
      /*
  def nextCount(): String = {
    val n = currCnt.value
    currCnt := n + 1
    String.format("%06d", Array(new java.lang.Integer(n)))
  }*/
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ListBuffer[(NoticeType.Value, NodeSeq)])(f)
  }
  
  /**
   * Initialize the current request session
   */
  def init[B](request : RequestState, session: LiftSession, vsh: VarStateHolder)(f: => B) : B = {
    _servletRequest.doWith(request.request) {
    _oldNotice.doWith(Nil) {
      _init(request,session, vsh)(() => f)
    }
    }
  }
  
  def session: Can[LiftSession] = _sessionInfo.value match {
    case null => Empty
    case s => Full(s)
  }
  
  def logQuery(query: String, time: Long) {
    _queryLog.value match {
      case null =>
      case lb => lb += (query, time)
    }
  }
  
  private var _queryAnalyzer: List[(Can[RequestState], Long, List[(String, Long)]) => Any] = Nil
  
  def addAnalyzer(f: (Can[RequestState], Long, List[(String, Long)]) => Any): Unit = _queryAnalyzer = _queryAnalyzer ::: List(f)
  
  private var aroundRequest: List[LoanWrapper] = Nil
  private def doAround[B](ar: List[LoanWrapper], f: => B): B = 
    ar match {
    case Nil => f
    case x :: xs => x(doAround(xs, f))
  }
  
  def addAround(lw: List[LoanWrapper]): Unit = aroundRequest = lw ::: aroundRequest 
  
  def queryLog: List[(String, Long)] = _queryLog.value match {
    case null => Nil
    case lb => lb.toList
  }
  
  private def wrapQuery[B](f:() => B): B = {
    _queryLog.doWith(new ListBuffer) {
      val begin = millis
      try {
      f()
      } finally {
        val log = queryLog
        val req = request match{ case null => Empty case s => Full(s)}
        val time = millis - begin
        _queryAnalyzer.foreach(_(req, time, log))
      }
    }
  }
  
  private def _init[B](request: RequestState, session: LiftSession, vsh: VarStateHolder)(f: () => B): B = {
    doAround(aroundRequest,
    _sessionInfo.doWith(session) (
    _stateInfo.doWith(vsh) {
      _requestVar.doWith(new HashMap) {
    _attrs.doWith(new HashMap) {
    snippetMap.doWith(new HashMap) {
      inS.doWith(true) {
          initNotice {
            _functionMap.doWith(new HashMap[String, AFuncHolder]) {
              this._request.doWith(request) {
                wrapQuery {
                this.currCnt.doWith(0)(f)
                }
              }
            }
          }
        }
      }
    }
      }
    }) )
    
  }
  
  private[http] object requestState {
    def apply[T](name: String): Can[T] = _requestVar.value match {
      case null => Empty
      case v => v.get(name) match {
        case Some(v: T) => Full(v)
        case _ => Empty
      }
    }
    
    def update[T](name: String, value: T) {_requestVar.value match {
      case null =>
      case v => v(name) = value
    }
    }
  }
  
  object state {
    def apply(name: String): Can[String] = _stateInfo.value match {
      case null => Empty
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
    def apply(what: String): Can[String] = Can(S._attrs.value.get(what))
    def update(what: String, value: String) = S._attrs.value(what) = value
  }
  
  def setVars[T](attr: MetaData)(f: => T): T = {
    val old = _attrs.value
    val ht: HashMap[String, String] = new HashMap
    old.elements.foreach(v => ht(v._1) = v._2)
    attr.elements.foreach(e => ht(e.key) = e.value.text)
    _attrs.doWith(ht)(f)
  }
  
  def initIfUninitted[B](session: LiftSession, vsh: VarStateHolder)(f: => B) : B = {
    if (inS.value) f
    else init(RequestState.nil,session, vsh)(f)
  }
  
  def init[B](request: RequestState, servletRequest: HttpServletRequest, oldNotices: Seq[(NoticeType.Value, NodeSeq)], session: LiftSession, vsh: VarStateHolder)(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
      this._servletRequest.doWith(servletRequest) {
        _init(request, session, vsh)(() => f)
      }
    }
  }
  
  def get(what: String): Can[String] = {
    val sr = _servletRequest.value
    if (sr eq null) Empty
    else {
      val ses = sr.getSession
      if (ses eq null) Empty
      else {
      val ret = ses.getAttribute(what)
      if (ret eq null) Empty
      else if (ret.isInstanceOf[String]) Full(ret.toString)
      else Empty
      }
    }
  }
  
  def servletSession: Can[HttpSession] = _servletRequest.value match {
    case null => Empty
    case r => Full(r.getSession)
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
  
  def hostName: String = servletRequest match {
    case null => "nowhere_123.com"
    case r => r.getServerName 
  }
  
  def hostAndPath: String = servletRequest match {
    case null => ""
    case r => r.getScheme+"://"+r.getServerName+":"+r.getServerPort+r.getContextPath
  }
  
  def functionMap: Map[String, AFuncHolder] = {
    _functionMap.value match {
      case null => Map.empty
      case s => Map(s.elements.toList :_*)
    }
  }
  
  def contextPath = session.map(_.contextPath).openOr("")  
  
  def locateSnippet(name: String): Can[NodeSeq => NodeSeq] = Can(snippetMap.value.get(name)) or {
    val snippet = name.split(":").toList.map(_.trim).filter(_.length > 0)
    if (LiftServlet.snippetTable.isDefinedAt(snippet)) Full(LiftServlet.snippetTable(snippet)) else Empty
  }
  
  def mapSnippet(name: String, func: NodeSeq => NodeSeq) {snippetMap.value(name) = func}

  
  def addFunctionMap(name: String, value: AFuncHolder) {
    _functionMap.value += (name -> value)
  }
  
  /*
  def mapFunction(name: String, f: AFuncHolder): String = {
    val ret = ""+nextCount()+"_"+name+"_"+randomString(5)
    _functionMap.value += (ret -> f)
    ret
  }*/

  private def booster(lst: List[String], func: String => Any): Boolean  = {
    lst.foreach(v => func(v))
    true
  }
  
  sealed abstract class FormElementPieces
  case class Id(name: String) extends FormElementPieces
  case class Cls(name: String) extends FormElementPieces
  // case class Val(value: String) extends FormElementPieces
  
  private def wrapFormElement(in: Elem, params: List[FormElementPieces]): Elem = {
    params match {
      case Nil => in
      case Id(name) :: rs => wrapFormElement(in % new UnprefixedAttribute("id", name, Null), rs)
      case Cls(name) :: rs => wrapFormElement(in % new UnprefixedAttribute("class", name, Null), rs)
    }
  }
  
  private def makeFormElement(name: String, func: AFuncHolder, params: Seq[FormElementPieces]): Elem =
    wrapFormElement( (<input type={name} name={mapFunc(func)}/>) , params.toList)
 
  /**
    * create an anchor tag around a body which will do an AJAX call and invoke the function
    *
    * @param func - the function to invoke when the link is clicked
    * @param body - the NodeSeq to wrap in the anchor tag
    */
  def a(func: () => Any, body: NodeSeq): Elem = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(key, (a: List[String]) => func())
    (<lift:a key={key}>{body}</lift:a>)
  }
    
    def a(body: NodeSeq)(func: => Any): Elem = a(() => func, body)
    
    def a(body: NodeSeq, cmd: JsCmd*): Elem = (<a href="#" onclick={cmd.map(_.toJsCmd).mkString(" ") + "return false;"}>{body}</a>)

    def span(body: NodeSeq, cmd: JsCmd*): Elem = (<span onclick={cmd.map(_.toJsCmd).mkString(" ")}>{body}</span>)
    
    /**
      * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
      * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
      * @param func -- the function to call when the data is sent
      *
      * @return the JavaScript that makes the call
      */
    def ajaxCall(jsCalcValue: String, func: String => Any): String = ajaxCall_*(jsCalcValue, SFuncHolder(func))
    
    /**
      * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
      * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
      * @param func -- the function to call when the data is sent
      *
      * @return the JavaScript that makes the call
      */
    def ajaxCall_*(jsCalcValue: String, func: AFuncHolder): String =
      "jQuery.ajax( {url: '"+
        contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+mapFunc(func)+"='+encodeURIComponent("+jsCalcValue+"), dataType: 'script'});"
    
    def toggleKids(head: Elem, visible: Boolean, func: () => Any, kids: Elem): NodeSeq = {
      val funcName = mapFunc(func)
      val (nk, id) = findOrAddId(kids)
      val rnk = if (visible) nk else nk % ("style" -> "display: none") 
      val nh = head % ("onclick" -> ("jQuery('#"+id+"').toggle(); jQuery.ajax( {url: '"+
        contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"=true', dataType: 'script'});"))
      nh ++ rnk
    }
    
    def ajaxText(value: String, func: String => Any, params: FormElementPieces*): Elem =
      ajaxText_*(value, SFuncHolder(func), params :_*)
      
    def ajaxText_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = {
      val funcName = mapFunc(func) 
      wrapFormElement(<input type="text" value={value}/>, params.toList) %
        ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
        ("onblur" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+this.value, dataType: 'script'});"))
    }
    
    def ajaxCheckbox(value: Boolean, func: String => Any, params: FormElementPieces*): Elem =
      ajaxCheckbox_*(value, SFuncHolder(func), params :_*)
      
    def ajaxCheckbox_*(value: Boolean, func: AFuncHolder, params: FormElementPieces*): Elem = {
      val funcName = mapFunc(func)
      wrapFormElement(<input type="checkbox"/>, params.toList) % checked(value) %
        ("onclick" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+this.checked, dataType: 'script'});"))        
    }
    
    def ajaxSelect(opts: List[(String, String)], deflt: Can[String], func: String => Any, params: FormElementPieces*): Elem = 
      ajaxSelect_*(opts, deflt, SFuncHolder(func), params :_*)
      
    def ajaxSelect_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder, params: FormElementPieces*): Elem = {
      val vals = opts.map(_._1)
      val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
      val funcName = mapFunc(testFunc)
      
      wrapFormElement((<select>{
        opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
      }</select>), params.toList) % ("onchange" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+this.options[this.selectedIndex].value, dataType: 'script'});"))
    }
    
    def ajaxInvoke(func: () => Any): String = "jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+
      mapFunc(NFuncHolder(func))+"=true', dataType: 'script'});"
    
    def findOrAddId(in: Elem): (Elem, String) = (in \ "@id").toList match {
      case Nil => val id = "R"+randomString(12)
      (in % ("id" -> id), id)
      case x :: xs => (in, x.text)
    } 
    
    /**
      *  Build a swappable visual element.  If the shown element is clicked on, it turns into the hidden element and when
      * the hidden element blurs, it swaps into the shown element.
      */
    def swappable(shown: Elem, hidden: Elem): Elem = {
      val (rs, sid) = findOrAddId(shown)
      val (rh, hid) = findOrAddId(hidden)
      (<span>{rs % ("onclick" -> ("jQuery('#"+sid+"').hide(); jQuery('#"+hid+"').show().each(function(i) {var t = this; setTimeout(function() { t.focus(); }, 200);}); return false;"))}{
        dealWithBlur(rh % ("style" -> "display: none"), ("jQuery('#"+sid+"').show(); jQuery('#"+hid+"').hide();"))}</span>)
    }
    
    def swappable(shown: Elem, hidden: String => Elem): Elem = {
      val (rs, sid) = findOrAddId(shown)
      val hid = "S"+randomString(10)
      val rh = <span id={hid}>{hidden("jQuery('#"+sid+"').show(); jQuery('#"+hid+"').hide();")}</span>
      
      (<span>{rs % ("onclick" -> ("jQuery('#"+sid+"').hide(); jQuery('#"+hid+"').show(); return false;"))}{
        (rh % ("style" -> "display: none"))}</span>)      
    }
    
    private def dealWithBlur(elem: Elem, blurCmd: String): Elem = {
      (elem \ "@onblur").toList match {
        case Nil => elem % ("onblur" -> blurCmd)
        case x :: xs => val attrs = elem.attributes.filter(_.key != "onblur")
        Elem(elem.prefix, elem.label, new UnprefixedAttribute("onblur", Text(blurCmd + x.text), attrs), elem.scope, elem.child :_*)
      }
    }
    

    /**
       * create an anchor tag around a body 
       *
       * @param func - the function to invoke when the link is clicked
       * @param body - the NodeSeq to wrap in the anchor tag
       */
     def link(to: String, func: () => Any, body: NodeSeq): Elem = {
       val key = "F"+System.nanoTime+"_"+randomString(3)
       addFunctionMap(key, (a: List[String]) => {func(); true})
       (<a href={to+"?"+key+"=_"}>{body}</a>)
     }
    
    def text_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = makeFormElement("text", func, params) % new UnprefixedAttribute("value", value, Null)
    def password_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = makeFormElement("password", func, params) % new UnprefixedAttribute("value", value, Null)
    def hidden_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = makeFormElement("hidden", func, params) % new UnprefixedAttribute("value", value, Null)
    def submit_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = makeFormElement("submit", func, params) % new UnprefixedAttribute("value", value, Null)
  def text(value: String, func: String => Any, params: FormElementPieces*): Elem = makeFormElement("text", SFuncHolder(func), params) % new UnprefixedAttribute("value", value, Null)
  def password(value: String, func: String => Any, params: FormElementPieces*): Elem = makeFormElement("password", SFuncHolder(func), params) % new UnprefixedAttribute("value", value, Null)
  def hidden(value: String, func: String => Any, params: FormElementPieces*): Elem = makeFormElement("hidden", SFuncHolder(func), params) % new UnprefixedAttribute("value", value, Null)
  def submit(value: String, func: String => Any, params: FormElementPieces*): Elem = makeFormElement("submit", SFuncHolder(func), params) % new UnprefixedAttribute("value", value, Null)
  
  def ajaxForm(func: => NodeSeq) = (<lift:form>{func}</lift:form>)
  def ajaxForm(onSubmit: JsCmd, func: => NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{func}</lift:form>)
  // List[value, display]
  def select(opts: List[(String, String)], deflt: Can[String], func: String => Any, params: FormElementPieces*): Elem = 
    select_*(opts, deflt, SFuncHolder(func), params :_*)
    
  def select_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder, params: FormElementPieces*): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    
    wrapFormElement((<select name={mapFunc(testFunc)}>{
      opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
    }</select>), params.toList)
  }
    
    
    private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "true", Null) else Null
    
     def multiSelect(opts: List[(String, String)], deflt: List[String], func: String => Any, params: FormElementPieces*): Elem = 
       multiSelect_*(opts, deflt, SFuncHolder(func), params :_*)

     def multiSelect_*(opts: List[(String, String)], deflt: List[String],func: AFuncHolder, params: FormElementPieces*): Elem =  
    wrapFormElement((<select multiple="true" name={mapFunc(func)}>{
      opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))
    }</select>), params.toList)
    

    def textarea(value: String, func: String => Any, params: FormElementPieces*): Elem = textarea_*(value, SFuncHolder(func), params :_*)
    
    def textarea_*(value: String, func: AFuncHolder, params: FormElementPieces*): Elem = wrapFormElement((<textarea name={mapFunc(func)}>{value}</textarea>), params.toList) 
    
    def radio(opts: List[String], deflt: Can[String], func: String => Any, params: FormElementPieces*): ChoiceHolder[String] =
      radio_*(opts, deflt, SFuncHolder(func), params :_*)
      
    def radio_*(opts: List[String], deflt: Can[String], func: AFuncHolder, params: FormElementPieces*): ChoiceHolder[String] = {
      val name = mapFunc(func)
      val itemList = opts.map(v => ChoiceItem(v, wrapFormElement((<input type="radio" name={name} value={v}/>) % 
        checked(deflt.filter((s: String) => s == v).isDefined), params.toList)))
      ChoiceHolder(itemList)
    }
    
  case class ChoiceItem[T](key: T, xhtml: NodeSeq)
    
  case class ChoiceHolder[T](items: List[ChoiceItem[T]]) {
      def apply(in: T) = items.filter(_.key == in).head.xhtml
      def apply(in: Int) = items(in).xhtml
      def map[A](f: ChoiceItem[T] => A) = items.map(f)
      def flatMap[A](f: ChoiceItem[T] => Iterable[A]) = items.flatMap(f)
      def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)
      def toForm: NodeSeq = flatMap(c => (<span>{c.xhtml}&nbsp;{c.key.toString}<br /></span>))
    }
  
  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null 
  
  def checkbox[T](possible: List[T], actual: List[T], func: List[T] => Any, params: FormElementPieces*): ChoiceHolder[T] = {
    val len = possible.length
    val name = mapFunc(LFuncHolder( (strl: List[String]) => {func(strl.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true}))
    // val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    
    ChoiceHolder(possible.zipWithIndex.map(p => 
      ChoiceItem(p._1, wrapFormElement((<input type="checkbox" name={name} value={p._2.toString}/>) % checked(actual.contains(p._1)),
          params.toList) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
  }
  
  def checkbox(value: Boolean, func: Boolean => Any, params: FormElementPieces*): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)), params :_*)
  }
    
  def checkbox_*(value: Boolean, func: AFuncHolder, params: FormElementPieces*): NodeSeq = {
    val name = mapFunc(func)
    // val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    (<input type="hidden" name={name} value="false"/>) ++
      wrapFormElement((<input type="checkbox" name={name} value="true" />) % checked(value), params.toList)
  }
  
  // implicit def toSFunc(in: String => Any): AFuncHolder = SFuncHolder(in)
  implicit def toLFunc(in: List[String] => Any): AFuncHolder = LFuncHolder(in, Empty)
  implicit def toNFunc(in: () => Any): AFuncHolder = NFuncHolder(in, Empty)
  // implicit def toL2Func(in: List[String] => AnyRef): AFuncHolder = L2FuncHolder(in)
  
  // implicit def stuffToUnpref(in: (String, Any)): UnprefixedAttribute = new UnprefixedAttribute(in._1, in._2.toString, Null)
  implicit def stuff2ToUnpref(in: (Symbol, Any)): UnprefixedAttribute = new UnprefixedAttribute(in._1.name, in._2.toString, Null)
  
  @serializable
  abstract class AFuncHolder {
    def owner: Can[String]
    def apply(in: List[String]): Any
    def duplicate(newOwner: String): AFuncHolder
  }

  object SFuncHolder {
    def apply(func: String => Any) = new SFuncHolder(func, Empty)
    def apply(func: String => Any, owner: Can[String]) = new SFuncHolder(func, owner)
  }
  
  @serializable
  class SFuncHolder(val func: String => Any,val owner: Can[String]) extends AFuncHolder {
    def this(func: String => Any) = this(func, Empty)
    def apply(in: List[String]): Any = in.map(func(_))
    def duplicate(newOwner: String) = new SFuncHolder(func, Full(newOwner))    
  }
  
  object LFuncHolder {
    def apply(func: List[String] => Any) = new LFuncHolder(func, Empty)
    def apply(func: List[String] => Any, owner: Can[String]) = new LFuncHolder(func, owner)
  }
  
  @serializable
  class LFuncHolder(val func: List[String] => Any,val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]): Any = func(in)
    def duplicate(newOwner: String) = new LFuncHolder(func, Full(newOwner))    
  }
  
  object NFuncHolder {
    def apply(func: () => Any) = new NFuncHolder(func, Empty)
    def apply(func: () => Any, owner: Can[String]) = new NFuncHolder(func, owner)
  }
  
  @serializable
  class NFuncHolder(val func: () => Any,val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]): Any = in.map(s => func())
    def duplicate(newOwner: String) = new NFuncHolder(func, Full(newOwner))        
  }
  
  def mapFunc(in: AFuncHolder): String = mapFunc("F"+System.nanoTime+"_"+randomString(3), in)
  
  def mapFunc(name: String, inf: AFuncHolder): String = {
    addFunctionMap(name, inf)
    name
  }
  
  
  def params(n: String) = request.params(n)
  def param(n: String): Can[String] = Can(request.param(n))
  
  def error(n: String) {error(Text(n))}
  def error(n: NodeSeq) {_notice.value += (NoticeType.Error, n)}
  def notice(n: String) {notice(Text(n))}
  def notice(n: NodeSeq) {_notice.value += (NoticeType.Notice, n)}
  def warning(n: String) {warning(Text(n))}
  def warning(n: NodeSeq) {_notice.value += (NoticeType.Warning, n)}
  
  def error(vi: List[ValidationIssue]) {_notice.value ++= vi.map{i => (NoticeType.Error, (<span><b>{i.field.name}</b>: {i.msg}</span>) )}}
  
  def getNotices = _notice.value.toList
  
  def errors: List[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Error).map(_._2))
  def notices: List[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Notice).map(_._2))
  def warnings: List[NodeSeq] = List(_oldNotice.value, _notice.value).flatMap(_.filter(_._1 == NoticeType.Warning).map(_._2))
  def clearCurrentNotices {_notice.value.clear}
  
}

object NoticeType extends Enumeration {
  val Notice, Warning, Error = Value
}

class VarStateHolder(val session: LiftSession, initVars: Map[String, String],setter: Can[Map[String, String] => Any], val local: Boolean) {
  //def this(s: LiftSession) = this(s, s.currentVars, false)
  
  private var vars = initVars
  
  def apply(name: String): Can[String] = Can(vars.get(name))
  def -=(name: String) {
    vars = vars - name
    setter.foreach(_(vars))
    if (local) session.stateVar - name
    else session ! UpdateState(name, Empty)
  }
  def update(name: String, value: String) {
    vars = vars + name -> value
    setter.foreach(_(vars))
    if (local) session.stateVar(name) = value
    else session ! UpdateState(name, Full(value))
  }
  
}

abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) { self: MyType =>
  private val name = "_lift_sv_"+getClass.getName // "V"+randomString(10)
  protected def findFunc: Can[String => Can[T]]
  protected def setFunc: Can[(String, T) => Unit]
  
  /**
    * The current value of the session variable
    */
  def is: T = findFunc.flatMap(_(name)).openOr{
      val ret = dflt
      this(ret)
      ret
    }
  
  /**
    * Set the session variable
    *
    * @param what -- the value to set the session variable to
    */
  def apply(what: T): MyType = {
    setFunc.foreach(_(name, what))
    this
  }  
}

/**
  * Keep session information around without the nastiness of naming session variables
  * or the type-unsafety of casting the results
  *
  * @param dflt - the default value of the session variable
  */
abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) {
  override protected def findFunc: Can[String => Can[T]] = S.servletSession.map(s => in => s.getAttribute(in) match {case Full(v: T) => Full(v) case _ => Empty})
  override protected def setFunc: Can[(String, T) => Unit] = S.servletSession.map(s => (in, what) => s.setAttribute(in, Full(what)))
}

/**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results
   *
   * @param dflt - the default value of the session variable
   */
abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) {
  override protected def findFunc: Can[String => Can[T]] = Full(name => S.requestState.apply[T](name))
  override protected def setFunc: Can[(String, T) => Unit] = Full((name, value) => S.requestState(name) = value)
}



object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is
  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
}
