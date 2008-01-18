package net.liftweb.http

/*                                                
 * (c) 2006-2008 WorldWide Conferencing, LLC
 * Distributed under an Apache License
 * http://www.apache.org/licenses/LICENSE-2.0
 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.xml.{NodeSeq, Elem, Text, UnprefixedAttribute, Null, MetaData, Group, Node}
import scala.collection.immutable.{ListMap, TreeMap}
import net.liftweb.util.{Helpers, ThreadGlobal, LoanWrapper, Can, Empty, Full, Failure, Log, JSONAny, JSONParser}
import net.liftweb.mapper.{Safe, ValidationIssue, MegaProtoUser}
import Helpers._
import js._
import java.io.InputStream
import java.util.{Locale, ResourceBundle}

/**
 * An object representing the current state of the HTTP request and response
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around
 */
object S {
  /**
    * Holds the partial function that re-write an incoming request
    */
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
  // private val _attrs = new ThreadGlobal[HashMap[String, String]]
  private val _attrs = new ThreadGlobal[Map[String, String]]
  private val _requestVar = new ThreadGlobal[HashMap[String, Any]]
  private val _sessionInfo = new ThreadGlobal[LiftSession]
  private val _queryLog = new ThreadGlobal[ListBuffer[(String, Long)]]
  private val _resBundle = new ThreadGlobal[Can[ResourceBundle]]
  private val _stateSnip = new ThreadGlobal[HashMap[String, StatefulSnippet]]
  
  /**
   * Get the current RequestState
   *
   * @return the current RequestState
   */
  def request: Can[RequestState] = _request.value match {case null => Empty case r => Full(r)}
  
  /**
    * Gets the list of templaters (partial functions that match and return a template rather than
    * loading a template from a file or a class)
    */
  def sessionTemplater: List[TemplateHolder] = servletSession.toList.flatMap(_.getAttribute(LiftServlet.SessionTemplateTableName) match {
    case rw: List[TemplateHolder] => rw
    case _ => Nil
  })

  
  /**
    * Returns the Action (uri) for the current render scope
    */
  // def action: String = request.map(_.uri).openOr("")
  
  /**
    * Returns session-specific request re-writers
    */
  def sessionRewriter: List[RewriteHolder] = servletSession.toList.flatMap(_.getAttribute(LiftServlet.SessionRewriteTableName) match {
  case rw: List[RewriteHolder] => rw
  case _ => Nil
  })
    
  /**
    * Returns a list of session-specific dispatchers
    */
  def sessionDispatcher: List[DispatchHolder] = servletSession.toList.flatMap(_.getAttribute(LiftServlet.SessionDispatchTableName) match {
  case rw: List[DispatchHolder] => rw
  case _ => Nil
  })

  /**
   * Returns the Locale for this request based on the HTTP request's 
   * Accept-Language header. If that header corresponds to a Locale
   * that's installed on this JVM then return it, otherwise return the
   * default Locale for this JVM.
   */
  def locale: Locale = LiftServlet.localeCalculator(request.map(_.request))

  private def reduxio(in: List[LiftServlet.DispatchPf]): LiftServlet.DispatchPf = in match {
    case Nil => Map.empty
    case x :: Nil => x
    case x :: xs => x orElse reduxio(xs)
  }
  
  def highLevelSessionDispatcher: LiftServlet.DispatchPf = reduxio(highLevelSessionDispatchList.map(_.dispatch))
  def highLevelSessionDispatchList: List[DispatchHolder] = servletSession.toList.flatMap(_.getAttribute(HighLevelSessionDispatchTableName) match {
    case li: List[DispatchHolder] => li
    case _ => Nil
  })
    
  val HighLevelSessionDispatchTableName = "$lift$__HighLelelDispatchTable__"
  def addHighLevelSessionDispatcher(name: String, disp: LiftServlet.DispatchPf) = 
    servletSession.foreach(_.setAttribute(HighLevelSessionDispatchTableName, DispatchHolder(name, disp) :: highLevelSessionDispatchList.filter(_.name != name)))
  
  def removeHighLevelSessionDispatcher(name: String) =
    servletSession.foreach(_.setAttribute(HighLevelSessionDispatchTableName, highLevelSessionDispatchList.filter(_.name != name)))
  
  def addSessionTemplater(name: String, rw: LiftServlet.TemplatePf) =
    servletSession.foreach(_.setAttribute(LiftServlet.SessionTemplateTableName, TemplateHolder(name, rw) :: sessionTemplater.filter(_.name != name)))
  
  def addSessionRewriter(name: String, rw: LiftServlet.RewritePf) =
  servletSession.foreach(_.setAttribute(LiftServlet.SessionRewriteTableName, RewriteHolder(name, rw) :: sessionRewriter.filter(_.name != name)))

  def removeSessionRewriter(name: String) =
    servletSession.foreach(_.setAttribute(LiftServlet.SessionRewriteTableName, sessionRewriter.remove(_.name == name)))
  
  def removeSessionTemplater(name: String) =
  servletSession.foreach(_.setAttribute(LiftServlet.SessionTemplateTableName, sessionTemplater.remove(_.name == name)))

  def addSessionDispatcher(name: String, rw: LiftServlet.DispatchPf) =
    servletSession.foreach(_.setAttribute(LiftServlet.SessionDispatchTableName, DispatchHolder(name, rw) :: sessionDispatcher.filter(_.name != name)))

  def removeSessionDispatcher(name: String) =
  servletSession.foreach(_.setAttribute(LiftServlet.SessionDispatchTableName, sessionDispatcher.remove(_.name == name)))
  
  /**
   * Test the current request to see if it's a POST
   */
  def post_? = request.map(_.post_?).openOr(false);

  /**
     * Localize the incoming string based on a resource bundle for the current locale
     * @param str the string or ID to localize (the default value is the
     *
     * @return the localized XML or Empty if there's no way to do localization
     */
   def loc(str: String): Can[NodeSeq] =
       resourceBundle.flatMap(r => tryo(r.getObject(str) match {
     case null => LiftServlet.localizationLookupFailureNotice.foreach(_(str, locale)); Empty
     case s: String => Full(LiftServlet.localizeStringToXml(s))
     case g: Group => Full(g)
     case e: Elem => Full(e)
     case n: Node => Full(n)
     case ns: NodeSeq => Full(ns)
     case x => Full(Text(x.toString))
   }).flatMap(s => s))

     /**
       * Get the resource bundle for the current locale
       */
     def resourceBundle: Can[ResourceBundle] = Can(_resBundle.value).openOr {
     val rb = tryo(ResourceBundle.getBundle(LiftServlet.resourceName, locale))
     _resBundle.set(rb)
     rb
     }

     /**
       * Get a localized string or return the original string
       *
       * @param str the string to localize
       *
       * @return the localized version of the string
       */
     def ?(str: String): String = resourceBundle.flatMap(r => tryo(r.getObject(str) match 
         {case s: String => Full(s) case _ => Empty}).flatMap(s => s)).
           openOr{LiftServlet.localizationLookupFailureNotice.foreach(_(str, locale)); str}
     
     def ?(str: String, params: Any *): String = if (params.length == 0) 
       ?(str)
     else
       String.format(locale, ?(str), params.flatMap{case s: AnyRef => List(s) case _ => Nil}.toArray) 

  /**
    * Localize the incoming string based on a resource bundle for the current locale
    * @param str the string or ID to localize
    * @param dflt the default string to return if localization fails
    *
    * @return the localized XHTML or default value
    */
  def loc(str: String, dflt: NodeSeq): NodeSeq = loc(str).openOr(dflt)

      
  /**
   * Test the current request to see if it's a GET
   */
  def get_? = request.map(_.get_?).openOr(false)

   /**
     * The URI of the current request (not re-written)
     */
  def uri: String = request.map(_.uri).openOr("/")
  
  /**
    * Redirect the browser to a given URL
    */
  def redirectTo[T](where: String): T = throw new RedirectException("Not Found", where)
  
  private val executionInfo = new ThreadGlobal[HashMap[String, Function[Array[String], Any]]]
  
  private val currCnt = new ThreadGlobal[Int]
  
  private def initNotice[B](f: => B): B = {
    _notice.doWith(new ListBuffer[(NoticeType.Value, NodeSeq)])(f)
  }
  
  /**
   * Initialize the current request session
   */
  def init[B](request: RequestState, session: LiftSession)(f: => B) : B = {
    _servletRequest.doWith(request.request) {
    _oldNotice.doWith(Nil) {
      _init(request,session)(() => f)
    }
    }
  }

  /**
    * The current LiftSession
    */
  def session: Can[LiftSession] = _sessionInfo.value
  
  /**
    * Log a query for the given request.  The query log can be tested to see
    * if queries for the particular page rendering took too long
    */
  def logQuery(query: String, time: Long) = Can(_queryLog.value).foreach(_ += (query, time))
  
  private[http] def snippetForClass(cls: String): Can[StatefulSnippet] =
    Can(_stateSnip.value).flatMap(_.get(cls))
  
  private[http] def setSnippetForClass(cls: String, inst: StatefulSnippet): Unit = 
    Can(_stateSnip.value).foreach(_(cls) = inst)
  
  private var _queryAnalyzer: List[(Can[RequestState], Long, List[(String, Long)]) => Any] = Nil

  /**
    * Add a query analyzer (passed queries for analysis or logging)
    */
  def addAnalyzer(f: (Can[RequestState], Long, List[(String, Long)]) => Any): Unit = _queryAnalyzer = _queryAnalyzer ::: List(f)
  
  private var aroundRequest: List[LoanWrapper] = Nil
  private def doAround[B](ar: List[LoanWrapper], f: => B): B = 
    ar match {
    case Nil => f
    case x :: xs => x(doAround(xs, f))
  }
  
  /**
    * You can wrap the handling of an HTTP request with your own wrapper.  The wrapper can
    * execute code before and after the request is processed (but still have S scope).
    * This allows for query analysis, etc.
    */  
  def addAround(lw: List[LoanWrapper]): Unit = aroundRequest = lw ::: aroundRequest 
  
  /**
    * Get a list of the logged queries
    */
  def queryLog: List[(String, Long)] = Can(_queryLog.value).map(_.toList).openOr(Nil)
  
  private def wrapQuery[B](f:() => B): B = {
    _queryLog.doWith(new ListBuffer) {
      val begin = millis
      try {
      f()
      } finally {
        val time = millis - begin
        _queryAnalyzer.foreach(_(request, time, queryLog))
      }
    }
  }
  
  private def _init[B](request: RequestState, session: LiftSession)(f: () => B): B = {
    doAround(aroundRequest,
    _sessionInfo.doWith(session) (
      _requestVar.doWith(new HashMap) {
    _attrs.doWith(new TreeMap) {
    snippetMap.doWith(new HashMap) {
      _resBundle.doWith(null) {
      inS.doWith(true) {
        _stateSnip.doWith(new HashMap) {
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
      }
    }) )
    
  }
  
  private[http] object requestState {
    private def rv: Can[HashMap[String, Any]] = _requestVar.value //  match {case null => Empty case v => Full(v)}
    
    def apply[T](name: String): Can[T] = rv.flatMap(r => Can(r.get(name).asInstanceOf[Option[T]]))
    
    def update[T](name: String, value: T): Unit = rv.foreach(_(name) = value)
    
    def clear(name: String): Unit = rv.foreach(_ -= name) 
  }
  
  object attr {
    def apply(what: String): Can[String] = Can(S._attrs.value.get(what))
    def update(what: String, value: String) = S._attrs.value(what) = value
  }
  
  def setVars[T](attr: MetaData)(f: => T): T = {
    //val old = _attrs.value
    //val ht: HashMap[String, String] = new HashMap
    //old.elements.foreach(v => ht(v._1) = v._2)
    //attr.elements.foreach(e => ht(e.key) = e.value.text)
    _attrs.doWith(_attrs.value ++ attr.toList.map(m => (m.key, m.value.text)))(f)
  }
  
  def initIfUninitted[B](session: LiftSession)(f: => B) : B = {
    if (inS.value) f
    else init(RequestState.nil,session)(f)
  }
  
  def init[B](request: RequestState, servletRequest: HttpServletRequest, oldNotices: Seq[(NoticeType.Value, NodeSeq)], session: LiftSession)(f : => B) : B = {
    _oldNotice.doWith(oldNotices) {
      this._servletRequest.doWith(servletRequest) {
        _init(request, session)(() => f)
      }
    }
  }
  
  def get(what: String): Can[String] = servletSession.flatMap(_.getAttribute(what) match {case s: String => Full(s) case _ => Empty}) 


  def servletSession: Can[HttpSession] = session.map(_.httpSession).or(_servletRequest.value match {
    case null => Empty
    case r => Full(r.getSession)
  })


  
  // def invokeSnippet[B](snippetName: String)(f: => B):B = _invokedAs.doWith(snippetName)(f)
  def invokedAs: String = _attrs.value.get("type") getOrElse ""
  
  def set(name: String, value: String) = servletSession.foreach(_.setAttribute(name, value)) 

  
  def unset(name: String) = servletSession.foreach(_.removeAttribute(name))

  /**
    * The current servlet request
    */
  def servletRequest: Can[HttpServletRequest] = Can(_servletRequest.value)
  
  /**
    * The host that the request was made on
    */
  def hostName: String = servletRequest.map(_.getServerName).openOr("nowhere_123.com")

  /**
    * The host and path of the quest
    */
  def hostAndPath: String = servletRequest.map(r => r.getScheme + "://"+r.getServerName+":"+r.getServerPort+r.getContextPath).openOr("")

  /**
    * Get a map of the name/functions
    */
  def functionMap: Map[String, AFuncHolder] = Can(_functionMap.value).map(s => Map(s.elements.toList :_*)).openOr(Map.empty)
  
  /**
    * The current context path
    */
  def contextPath = session.map(_.contextPath).openOr("")  
  
  def locateSnippet(name: String): Can[NodeSeq => NodeSeq] = Can(snippetMap.value.get(name)) or {
    val snippet = if (name.indexOf(".") != -1) name.roboSplit(".") else name.roboSplit(":") // name.split(":").toList.map(_.trim).filter(_.length > 0)
    if (LiftServlet.snippetTable.isDefinedAt(snippet)) Full(LiftServlet.snippetTable(snippet)) else Empty
  }
  
  def mapSnippet(name: String, func: NodeSeq => NodeSeq) {snippetMap.value(name) = func}

  
  def addFunctionMap(name: String, value: AFuncHolder) = _functionMap.value += (name -> value)


  private def booster(lst: List[String], func: String => Any): Unit = lst.foreach(v => func(v))
  
  private def makeFormElement(name: String, func: AFuncHolder): Elem = (<input type={name} name={mapFunc(func)}/>)
 
  
  /**
    * Create an Ajax button. When it's pressed, the function is executed
    *
    * @param text -- the name/text of the button
    * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
    *
    * @return a button to put on your page
    */
  def ajaxButton(func: => JsCmd, text: String): Elem =
    <input type="button" value={text}/> % 
      ("onclick" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+
        mapFunc(() => func)+"=true', dataType: 'script'});"))

  /**
    * Create an Ajax button. When it's pressed, the function is executed
    *
    * @param text -- the name/text of the button
    * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
    *
    * @return a button to put on your page
    */
  def ajaxButton(text: String)(func: => JsCmd): Elem =
    <input type="button" value={text}/> % 
      ("onclick" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+
        mapFunc(() => func)+"=true', dataType: 'script'});"))
        
  def buildJSONFunc(f: JSONAny => JsCmd): (String, JsCmd) = {
      val key = "F"+System.nanoTime+"_"+randomString(3)

      def jsonCallback(in: List[String]): JsCmd = 
        in.flatMap(s => JSONParser.parse(s).map(List(_)).getOrElse(Nil).map(f)).foldLeft(JsCmds.Noop)(_ ++ _)

      addFunctionMap(key, jsonCallback _)
      
      (key, JsCmds.Run("function "+key+"(obj) {jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+
        key+"='+encodeURIComponent(JSON.stringify(obj)) , dataType: 'script'});}")) // HERE
    }
        
  /**
    * create an anchor tag around a body which will do an AJAX call and invoke the function
    *
    * @param func - the function to invoke when the link is clicked
    * @param body - the NodeSeq to wrap in the anchor tag
    */
  def a(func: () => JsCmd, body: NodeSeq): Elem = {
    val key = "F"+System.nanoTime+"_"+randomString(3)
    addFunctionMap(key, (a: List[String]) => func())
    (<lift:a key={key}>{body}</lift:a>)
  }
    
    def a(body: NodeSeq)(func: => JsCmd): Elem = a(() => func, body)
    
    /**
      * Create an anchor that will run a JavaScript command when clicked
      */
    def a(body: NodeSeq, cmd: JsCmd): Elem = (<a href="#" onclick={cmd.toJsCmd + "; return false;"}>{body}</a>)

    /**
      * Create a span that will run a JavaScript command when clicked
      */
    def span(body: NodeSeq, cmd: JsCmd): Elem = (<span onclick={cmd.toJsCmd}>{body}</span>)
    
    /**
      * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
      * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
      * @param func -- the function to call when the data is sent
      *
      * @return the JavaScript that makes the call
      */
    def ajaxCall(jsCalcValue: String, func: String => JsCmd): String = ajaxCall_*(jsCalcValue, SFuncHolder(func))
    
    /**
      * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
      * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
      * @param func -- the function to call when the data is sent
      *
      * @return the JavaScript that makes the call
      */
    private def ajaxCall_*(jsCalcValue: String, func: AFuncHolder): String =
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
    
    def ajaxText(value: String, func: String => JsCmd): Elem = ajaxText_*(value, SFuncHolder(func))
      
    private def ajaxText_*(value: String, func: AFuncHolder): Elem = {
      val funcName = mapFunc(func) 
      (<input type="text" value={value}/>) %
        ("onkeypress" -> """var e = event ; var char = ''; if (e && e.which) {char = e.which;} else {char = e.keyCode;}; if (char == 13) {this.blur(); return false;} else {return true;};""") %
        ("onblur" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+encodeURIComponent(this.value), dataType: 'script'});"))
    }
    
    def ajaxCheckbox(value: Boolean, func: String => JsCmd): Elem = ajaxCheckbox_*(value, SFuncHolder(func))
      
    private def ajaxCheckbox_*(value: Boolean, func: AFuncHolder): Elem = {
      val funcName = mapFunc(func)
      (<input type="checkbox"/>) % checked(value) %
        ("onclick" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+this.checked, dataType: 'script'});"))        
    }
    
    def ajaxSelect(opts: List[(String, String)], deflt: Can[String], func: String => JsCmd): Elem = ajaxSelect_*(opts, deflt, SFuncHolder(func))
      
    private def ajaxSelect_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
      val vals = opts.map(_._1)
      val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
      val funcName = mapFunc(testFunc)
      
      (<select>{
        opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
      }</select>) % ("onchange" -> ("jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+funcName+"='+this.options[this.selectedIndex].value, dataType: 'script'});"))
    }
    
    def ajaxInvoke(func: () => JsCmd): String = "jQuery.ajax( {url: '"+contextPath+"/"+LiftServlet.ajaxPath+"', cache: false, data: '"+
      mapFunc(NFuncHolder(func))+"=true', dataType: 'script'});"
    
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
    
    def text_*(value: String, func: AFuncHolder): Elem = makeFormElement("text", func) % new UnprefixedAttribute("value", value, Null)
    def password_*(value: String, func: AFuncHolder): Elem = makeFormElement("password", func) % new UnprefixedAttribute("value", value, Null)
    def hidden_*(func: AFuncHolder): Elem = makeFormElement("hidden", func) % ("value" -> "true")
    def submit_*(value: String, func: AFuncHolder): Elem = makeFormElement("submit", func) % new UnprefixedAttribute("value", value, Null)
  def text(value: String, func: String => Any): Elem = makeFormElement("text", SFuncHolder(func)) % new UnprefixedAttribute("value", value, Null)
  def password(value: String, func: String => Any): Elem = makeFormElement("password", SFuncHolder(func)) % new UnprefixedAttribute("value", value, Null)
  def hidden(func: String => Any): Elem = makeFormElement("hidden", SFuncHolder(func)) % ("value" -> "true")
  def submit(value: String, func: String => Any): Elem = makeFormElement("submit", SFuncHolder(func)) % new UnprefixedAttribute("value", value, Null)
  
  def ajaxForm(body: NodeSeq) = (<lift:form>{body}</lift:form>)
  def ajaxForm(onSubmit: JsCmd, body: NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)

  /**
     * Create a select box based on the list with a default value and the function to be executed on
     * form submission
     *
     * @param opts -- the options.  A list of value and text pairs
     * @param deflt -- the default value (or Empty if no default value)
     * @param func -- the function to execute on form submission
     */
   def select(opts: List[(String, String)], deflt: Can[String], func: String => Any): Elem = select_*(opts, deflt, SFuncHolder(func))
     
   /**
      * Create a select box based on the list with a default value and the function to be executed on
      * form submission
      *
      * @param opts -- the options.  A list of value and text pairs
      * @param deflt -- the default value (or Empty if no default value)
      * @param func -- the function to execute on form submission
      */
   def select_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
     val vals = opts.map(_._1)
     val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
     
     (<select name={mapFunc(testFunc)}>{
       opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
     }</select>)
   }
     
    /**
       * Create a select box based on the list with a default value and the function to be executed on
       * form submission.  No check is made to see if the resulting value was in the original list.
       * For use with DHTML form updating.
       *
       * @param opts -- the options.  A list of value and text pairs
       * @param deflt -- the default value (or Empty if no default value)
       * @param func -- the function to execute on form submission
       */
     def untrustedSelect(opts: List[(String, String)], deflt: Can[String], func: String => Any): Elem = untrustedSelect_*(opts, deflt, SFuncHolder(func))
       
     /**
       * Create a select box based on the list with a default value and the function to be executed on
       * form submission.  No check is made to see if the resulting value was in the original list.
       * For use with DHTML form updating.
        *
        * @param opts -- the options.  A list of value and text pairs
        * @param deflt -- the default value (or Empty if no default value)
        * @param func -- the function to execute on form submission
        */
     def untrustedSelect_*(opts: List[(String, String)],deflt: Can[String], func: AFuncHolder): Elem = {
       (<select name={mapFunc(func)}>{
         opts.flatMap{case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}
       }</select>)
     }
           
    
    private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "true", Null) else Null
    
     def multiSelect(opts: List[(String, String)], deflt: List[String], func: String => Any): Elem = multiSelect_*(opts, deflt, SFuncHolder(func))

     def multiSelect_*(opts: List[(String, String)], deflt: List[String],func: AFuncHolder): Elem = (<select multiple="true" name={mapFunc(func)}>{
      opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))
    }</select>)
    

    def textarea(value: String, func: String => Any): Elem = textarea_*(value, SFuncHolder(func))
    
    def textarea_*(value: String, func: AFuncHolder): Elem = (<textarea name={mapFunc(func)}>{value}</textarea>) 
    
    def radio(opts: List[String], deflt: Can[String], func: String => Any): ChoiceHolder[String] =
      radio_*(opts, deflt, SFuncHolder(func))
      
    def radio_*(opts: List[String], deflt: Can[String], func: AFuncHolder): ChoiceHolder[String] = {
      val name = mapFunc(func)
      val itemList = opts.map(v => ChoiceItem(v, (<input type="radio" name={name} value={v}/>) % 
        checked(deflt.filter((s: String) => s == v).isDefined)))
      ChoiceHolder(itemList)
    }
    
    def fileUpload(func: FileParamHolder => Any): Elem = <input type="file" name={mapFunc(BinFuncHolder(func))} />
    
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
  
  def checkbox[T](possible: List[T], actual: List[T], func: List[T] => Any): ChoiceHolder[T] = {
    val len = possible.length
    val name = mapFunc(LFuncHolder( (strl: List[String]) => {func(strl.map(toInt(_)).filter(x =>x >= 0 && x < len).map(possible(_))); true}))
    // val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    
    ChoiceHolder(possible.zipWithIndex.map(p => 
      ChoiceItem(p._1, (<input type="checkbox" name={name} value={p._2.toString}/>) % checked(actual.contains(p._1)) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
  }
  
  def checkbox(value: Boolean, func: Boolean => Any): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)))
  }
    
  def checkbox_*(value: Boolean, func: AFuncHolder): NodeSeq = {
    val name = mapFunc(func)
    // val realParams = params.toList.filter(p => p match {case Val(_) => false; case _ => true})
    (<input type="hidden" name={name} value="false"/>) ++
      ((<input type="checkbox" name={name} value="true" />) % checked(value))
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
  
  
  
  @serializable  
  class BinFuncHolder(val func: FileParamHolder => Any, val owner: Can[String]) extends AFuncHolder {
    def apply(in: List[String]) {Log.error("You attempted to call a 'File Upload' function with a normal parameter.  Did you forget to 'enctype' to 'multipart/form-data'?")}
    def apply(in: FileParamHolder) = func(in)
    def duplicate(newOwner: String) = new BinFuncHolder(func, Full(newOwner))
  }
  
  object BinFuncHolder {
    def apply(func: FileParamHolder => Any) = new BinFuncHolder(func, Empty)
    def apply(func: FileParamHolder => Any, owner: Can[String]) = new BinFuncHolder(func, owner)
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
  
  
  def params(n: String): List[String] = request.map(_.params(n)).openOr(Nil)
  def param(n: String): Can[String] = request.flatMap(r => Can(r.param(n)))
  
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

@serializable
object NoticeType extends Enumeration {
  val Notice, Warning, Error = Value
}

abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => Can[T]) { self: MyType =>
  private val name = "_lift_sv_"+getClass.getName // "V"+randomString(10)
  protected def findFunc(name: String): Can[T]
  protected def setFunc(name: String, value: T): Unit
  protected def clearFunc(name: String): Unit
  
  /**
    * The current value of the session variable
    */
  def is: Can[T] = findFunc(name) match {
    case v @ Full(_) => v
    case _ => val ret = dflt
    ret.foreach(v => apply(v))
    ret
  }
  
  /**
    * Set the session variable
    *
    * @param what -- the value to set the session variable to
    */
  def apply(what: T): Unit = setFunc(name, what)
  
  def apply(what: Can[T]): Unit = what match {
    case Full(w) => setFunc(name, w)
    case _ => clearFunc(name)
    //case Failure(_, _, _) => ()        // no-op (or clear?)
    //case Empty => clearFunc(name)      // clear (or no-op?)
  }
  
  def remove(): Unit = clearFunc(name)   
  
  override def toString = is.map(_.toString).openOr("Undefined")
}

/**
  * Keep session information around without the nastiness of naming session variables
  * or the type-unsafety of casting the results.
  * SessionVars are type-safe variables that map pretty directly to  
  * HttpSession attributes.  Put stuff in and they are available for the  
  * life of the Session.
  *
  * @param dflt - the default value of the session variable
  */
abstract class SessionVar[T](dflt: => Can[T]) extends AnyVar[T, SessionVar[T]](dflt) {
  override protected def findFunc(name: String): Can[T] = S.servletSession.flatMap(_.getAttribute(name) match {case Full(v: T) => Full(v) case _ => Empty})
  override protected def setFunc(name: String, value: T): Unit = S.servletSession.foreach(_.setAttribute(name, Full(value)))
  override protected def clearFunc(name: String): Unit = S.servletSession.foreach(_.removeAttribute(name))
}

/**
   * Keep request-local information around without the nastiness of naming session variables
   * or the type-unsafety of casting the results.
   * RequestVars share their value through the scope of the current HTTP  
   * request.  They have no value at the beginning of request servicing  
   * and their value is discarded at the end of request processing.  They  
   * are helpful to share values across many snippets.
   *
   * @param dflt - the default value of the session variable
   */
abstract class RequestVar[T](dflt: => Can[T]) extends AnyVar[T, RequestVar[T]](dflt) {
  override protected def findFunc(name: String): Can[T] = S.requestState(name) // S.servletSession.flatMap(_.getAttribute(name) match {case Full(v: T) => Full(v) case _ => Empty})
  override protected def setFunc(name: String, value: T): Unit = S.requestState(name) = value // S.servletSession.foreach(_.setAttribute(name, Full(value)))
  override protected def clearFunc(name: String): Unit = S.requestState.clear(name)
}



object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): Can[T] = in.is
  implicit def whatRequestVarIs[T](in: RequestVar[T]): Can[T] = in.is
}
