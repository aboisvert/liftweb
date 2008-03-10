/*
* Copyright 2007-2008 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions
* and limitations under the License.
*/
package net.liftweb.http;

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession, Cookie}
import javax.servlet.{ServletContext}
import scala.collection.mutable.{ListBuffer}
import java.net.URLDecoder
import scala.xml.{Node, NodeSeq,Group, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment, Text}
import scala.xml.transform._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import net.liftweb.mapper.DB
import net.liftweb.util._
import java.io.InputStream
import net.liftweb.util.Helpers
import net.liftweb.util.ActorPing
import net.liftweb.sitemap.SiteMap
import java.net.URL
import net.liftweb.sitemap._
import js._
import javax.servlet._
import javax.servlet.http._
import java.io.ByteArrayOutputStream
import java.util.{Locale, TimeZone, ResourceBundle}

/**
* An implementation of HttpServlet.  Just drop this puppy into 
* your Java web container, do a little magic in web.xml, and
* ta-da, you've got a scala-powered Servlet
* 
*/
private[http] class LiftServlet(val getServletContext: ServletContext) extends AnyRef /* HttpServlet */ {
  private val actorNameConst = "the_actor"
  private var requestCnt = 0
  
  def destroy = {
    try {
      LiftServlet.ending = true
      Scheduler.snapshot // pause the Actor scheduler so we don't have threading issues
      Scheduler.shutdown 
      ActorPing.shutdown
      Log.debug("Destroyed servlet")
      // super.destroy
    } catch {
      case e => Log.error("Servlet destruction failure",e)
    } finally {
      clearThread
    }
  }
  
  def init = {
    try {
      // if (Scheduler.tasks ne null) {Log.error("Restarting Scheduler"); Scheduler.restart} // restart the Actor scheduler
      LiftServlet.ending = false
      LiftServlet.addDispatchAfter({
        case RequestMatcher(r, ParsePath(mainPath :: subPath, _,_),_, _) if mainPath == LiftServlet.ResourceServerPath => ResourceServer.findResourceInClasspath(r, subPath)
      })      
      // ResourceServer.allow("/jquery-1.2.2.js")
      // super.init
    } finally {
      clearThread
    }
  }
  
  /**
  * Is the file an existing file in the WAR?
  */
  private def isExistingFile_?(request : HttpServletRequest) : Can[URL] = {
    if (!goodPath_?(request.getRequestURI)) Empty else
    getServletContext.getResource(request.getRequestURI.substring(request.getContextPath.length)) match {
      case null => Empty
      case u : URL => Full(u)
    }
  }
  
  def getActor(request: RequestState, session: HttpSession): LiftSession = {
    val ret = session.getValue(actorNameConst) match {
      case r: LiftSession => r
      case _ => 
      val ret = LiftSession(session, request.contextPath)
      // ret.start
      session.putValue(actorNameConst, ret)
      ret
    }
    ret.breakOutComet()
    ret
  }
  
  def service(req: HttpServletRequest,resp: HttpServletResponse, requestState: RequestState) {
    try {
      def doIt {
        logTime("Service request ("+req.getMethod+") "+req.getRequestURI) {
          doService(req, resp, requestState)
        }      
      }
      LiftServlet.checkJetty(req) match {
        case None => doIt
        case r if r eq null => doIt
        case r: ResponseIt => sendResponse(r.toResponse, resp, Empty)
        case Some(r: ResponseIt) => sendResponse(r.toResponse, resp, Empty)
        case _ => doIt
      }
    } catch {
      case e => Log.warn("Request for "+req.getRequestURI+" failed "+e.getMessage, e); throw new Exception("Request failed", e)
    } finally {
      clearThread
    }
  }
  
  private def flatten(in: List[Any]): List[AnyRef] = in match {
    case Nil => Nil
    case Some(x: AnyRef) :: xs => x :: flatten(xs)
    case Full(x: AnyRef) :: xs => x :: flatten(xs)
    case (lst: Iterable[AnyRef]) :: xs => lst.toList ::: flatten(xs)
    case (x: AnyRef) :: xs => x :: flatten(xs)
    case x :: xs => flatten(xs)
  }
  
  /**
  * Service the HTTP request
  */ 
  def doService(request: HttpServletRequest, response: HttpServletResponse, requestState: RequestState) {
    val statelessToMatch = RequestMatcher(requestState, requestState.path, RequestType(request), Empty)     
    
    val resp: Response = if (LiftServlet.ending) {
      requestState.createNotFound.toResponse
    } else if (LiftServlet.statelessDispatchTable.isDefinedAt(statelessToMatch)) {
      val f = LiftServlet.statelessDispatchTable(statelessToMatch)
      f(requestState) match {
        case Full(v) => LiftServlet.convertResponse( (v, Nil, S.responseCookies, requestState) )
        case Empty => requestState.createNotFound.toResponse
        case f: Failure => requestState.createNotFound(f).toResponse 
      }
    } else {
      val sessionActor = getActor(requestState, request.getSession)    
      val toMatch = RequestMatcher(requestState, requestState.path, RequestType(request), Full(sessionActor))
      if (LiftServlet.dispatchTable(request).isDefinedAt(toMatch)) {
        S.init(requestState, requestState.request, sessionActor.notices, sessionActor) {
          try {
            val f = LiftServlet.dispatchTable(request)(toMatch)
            f(requestState) match {
              case Full(v) => LiftServlet.convertResponse( (v, Nil, S.responseCookies, requestState) )
              case Empty => requestState.createNotFound.toResponse
              case f: Failure => requestState.createNotFound(f).toResponse 
            }
          } finally {
            sessionActor.notices = S.getNotices
          }
        }
      } else if (requestState.path.path.length == 1 && requestState.path.path.head == LiftServlet.cometPath) {
        
        LiftServlet.cometLogger.debug("Comet Request: "+sessionActor.uniqueId+" "+requestState.params)
        
        // sessionActor.breakOutComet()
        sessionActor.enterComet(self)
        try {
          S.init(requestState, sessionActor) {
            val actors: List[(CometActor, Long)] = requestState.params.toList.flatMap{case (name, when) => sessionActor.getAsyncComponent(name).toList.map(c => (c, toLong(when)))}
            
            def drainTheSwamp(len: Long, in: List[AnswerRender]): List[AnswerRender] = { // remove any message from the current thread's inbox
              receiveWithin(len) {
                case TIMEOUT => in
                case ar: AnswerRender => drainTheSwamp(0, ar :: in) 
                case BreakOut => drainTheSwamp(0, in)
                case s @ _ => Log.trace("Drained "+s) ; drainTheSwamp(0, in)
              }
            }
            
            drainTheSwamp(0, Nil)
            
            if (actors.isEmpty) new JsCommands(JsCmds.RedirectTo(LiftServlet.noCometSessionPage) :: Nil).toResponse
            else {
              
              actors.foreach{case (act, when) => act ! Listen(when)}
              
              val ret = drainTheSwamp((LiftServlet.ajaxRequestTimeout openOr 120) * 1000L, Nil) 
              
              actors.foreach{case (act, _) => act ! Unlisten}
              
              val ret2 = drainTheSwamp(100L, ret)
              
              val jsUpdateTime = ret2.map(ar => "lift_toWatch['"+ar.who.uniqueId+"'] = '"+ar.when+"';").mkString("\n")
              val jsUpdateStuff = ret2.map(ar => ar.response.toJavaScript(sessionActor, ar.displayAll))
              
              val all = jsUpdateStuff.reverse.foldLeft(JsCmds.Noop)(_ & _) & JE.JsRaw(jsUpdateTime).cmd
              
              
              LiftServlet.cometLogger.debug("Comet Request: "+sessionActor.uniqueId+" response: "+all.toJsCmd)
              
              (new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff)).toResponse
            }
          }
        } finally {
          sessionActor.exitComet(self)
        }
      } else if (requestState.path.path.length == 1 && requestState.path.path.head == LiftServlet.ajaxPath) {
        LiftServlet.cometLogger.debug("AJAX Request: "+sessionActor.uniqueId+" "+requestState.params)
        S.init(requestState, sessionActor) {
          try {
            val what = flatten(sessionActor.runParams(requestState))
            
            val what2 = what.flatMap{case js: JsCmd => List(js); case n: NodeSeq => List(n) case js: JsCommands => List(js)  case r: ResponseIt => List(r); case s => Nil}
            
            val ret = what2 match {
              case (n: Node) :: _ => XmlResponse(n).toResponse
              case (ns: NodeSeq) :: _ => XmlResponse(Group(ns)).toResponse
              case (r: ResponseIt) :: _ => r.toResponse
              case (js: JsCmd) :: xs  => (new JsCommands((js :: xs).flatMap{case js: JsCmd => List(js) case _ => Nil}.reverse)).toResponse
              case _ => (new JsCommands(JsCmds.Noop :: Nil)).toResponse
            }
            
            LiftServlet.cometLogger.debug("AJAX Response: "+sessionActor.uniqueId+" "+ret)
            ret
          } finally {
            sessionActor.updateFunctionMap(S.functionMap)
          }
        }
      } else {
        try {
          this.synchronized {
            this.requestCnt = this.requestCnt + 1
          }
          
          sessionActor.processRequest(requestState, request).what.toResponse
          
        } finally {
          this.synchronized {
            this.requestCnt = this.requestCnt - 1
            this.notifyAll
          }
        }
      }
    }
    logIfDump(requestState, resp)
    
    sendResponse(resp, response, Full(requestState))
  }
  
  val dumpRequestResponse = Props.getBool("dump.request.response")
  
  private def logIfDump(request: RequestState, response: Response) {
    if (dumpRequestResponse) {
      val toDump = request.uri+"\n"+
      request.params + "\n"+
      response.headers+"\n"+
      new String(response.data)
      
      Log.info(toDump)
    }
  }
  
  /**
  * Sends the {@code HttpServletResponse} to the browser using data from the 
  * {@link Response} and {@link RequestState}.
  */
  def sendResponse(resp: Response, response: HttpServletResponse, request: Can[RequestState]) {
    val bytes = resp.data
    val len = bytes.length
    // insure that certain header fields are set
    val header = insureField(resp.headers, List(("Content-Type", LiftServlet.determineContentType(request)),
    ("Content-Encoding", "UTF-8"),
    ("Content-Length", len.toString)))
    
    LiftServlet._beforeSend.foreach(_(resp, response, header, request))
    // set the cookies
    resp.cookies.foreach(cookie => response.addCookie(cookie))
    
    // send the response
    header.elements.foreach {case (name, value) => response.setHeader(name, value)}
    response setStatus resp.code
    response.getOutputStream.write(bytes)    
    LiftServlet._afterSend.foreach(_(resp, response, header, request))
  }
  
  /**
  * Remove any thread-local associations
  */
  def clearThread: Unit = {
    // uncomment for Scala 2.6.1 to avoid memory leak 
    // Actor.clearSelf
    DB.clearThread
  }
  
}

object LiftServlet {
  val SessionDispatchTableName = "$lift$__DispatchTable__"
  val SessionRewriteTableName = "$lift$__RewriteTable__"
  val SessionTemplateTableName = "$lift$__TemplateTable__"
  
  type DispatchPf = PartialFunction[RequestMatcher, RequestState => Can[ResponseIt]];
  type RewritePf = PartialFunction[RewriteRequest, RewriteResponse]
  type TemplatePf = PartialFunction[RequestMatcher,() => Can[NodeSeq]]
  type SnippetPf = PartialFunction[List[String], NodeSeq => NodeSeq]
  type LiftTagPF = PartialFunction[(String, Elem, MetaData, NodeSeq, String), NodeSeq]
  
  /**
  * A partial function that allows the application to define requests that should be
  * handled by lift rather than the default servlet handler
  */
  type LiftRequestPf = PartialFunction[RequestState, Boolean]
  
  private var _early: List[(HttpServletRequest) => Any] = Nil
  private[http] var _beforeSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendBeforeSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _beforeSend = _beforeSend ::: List(f)
  }
  
  /**
  * The path to handle served resources
  */
  var ResourceServerPath = "classpath" 
  
  private[http] var _afterSend: List[(Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any] = Nil
  
  def appendAfterSend(f: (Response, HttpServletResponse, List[(String, String)], Can[RequestState]) => Any) {
    _afterSend = _afterSend ::: List(f)
  }
  
  /**
  * Determine the proper Content-Type based on the browser's Accept HTTP Header.
  */
  def determineContentType(req: Can[RequestState]) : String = {
    req match {
      case Full(request) => determineContentType(request.request)
      case _ => "text/html"
    }
  }
  
  /**
  * The maximum allowed size of a complete mime multi-part POST.  Default
  * 8MB
  */
  var maxMimeSize: Long = 8 * 1024 * 1024
  
  /**
  * The maximum allowed size of a single file in a mime multi-part POST.
  * Default 7MB
  */
  var maxMimeFileSize: Long = 7 * 1024 * 1024
  
  /**
  * The function referenced here is called if there's a localization lookup failure
  */
  var localizationLookupFailureNotice: Can[(String, Locale) => Unit] = Empty
  
  def determineContentType(request: HttpServletRequest) : String = {
    request match {
      case null => "text/html"
      case request => determineContentType(request.getHeader("Accept"))
    }
  }
  
  var liftTagProcessing: LiftTagPF = Map.empty
  
  /**
  * If you don't want lift to send the application/xhtml+xml mime type to those browsers
  * that understand it, then set this to {@code false}
  */
  var useXhtmlMimeType: Boolean = true
  
  def determineContentType(accept: String) : String = {
    // If application/xhtml+xml is explicitly listed then let's use that.
    if (useXhtmlMimeType && accept != null && accept.contains("application/xhtml+xml")) {
      "application/xhtml+xml"
    } else {
      "text/html"
    }
  }
  
  
  private def _stringToXml(s: String): NodeSeq = Text(s)
  
  /**
  * A function that defines how a String should be converted to XML
  * for the localization stuff.  By default, Text(s) is returned,
  * but you can change this to attempt to parse the XML in the String and
  * return the NodeSeq. 
  */
  var localizeStringToXml: String => NodeSeq = _stringToXml _  
  
  /**
  * The base name of the resource bundle
  */
  var resourceName = "lift"
  
  /**
  * The base name of the resource bundle of the lift core code
  */
  var liftCoreResourceName = "i18n.lift-core"
  
  /**
  * Where to send the user if there's no comet session
  */
  var noCometSessionPage = "/"
  
  /**
  * Put a function that will calculate the request timeout based on the
  * incoming request.
  */
  var calcRequestTimeout: Can[RequestState => Int] = Empty
  
  /**
  * If you want the standard (non-AJAX) request timeout to be something other than
  * 10 seconds, put the value here
  */
  var stdRequestTimeout: Can[Int] = Empty
  
  /**
  * If you want the AJAX request timeout to be something other than 120 seconds, put the value here
  */
  var ajaxRequestTimeout: Can[Int] = Empty
  
  /**
  * If the request times out (or returns a non-Response) you can
  * intercept the response here and create your own response
  */
  var requestTimedOut: Can[(RequestState, Any) => Can[Response]] = Empty
  
  def early = {
    // test_boot
    _early
  }
  
  /**
  * A function that takes the current HTTP request and returns the current 
  */
  var timeZoneCalculator: Can[HttpServletRequest] => TimeZone = 
  defaultTimeZoneCalculator _
  
  def defaultTimeZoneCalculator(request: Can[HttpServletRequest]): TimeZone =
  TimeZone.getDefault
  
  
  /**
  * A function that takes the current HTTP request and returns the current 
  */
  var localeCalculator: Can[HttpServletRequest] => Locale = defaultLocaleCalculator _
  
  def defaultLocaleCalculator(request: Can[HttpServletRequest]) = request.flatMap(_.getLocale() match {case null => Empty case l: Locale => Full(l)}).openOr(Locale.getDefault())
  
  
  val (hasJetty_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
      val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
      val meth = cc.getMethod("getContinuation", Array(classOf[HttpServletRequest], classOf[AnyRef]))
      val cci = Class.forName("org.mortbay.util.ajax.Continuation")
      val getObj = cci.getMethod("getObject", null)
      val setObj = cci.getMethod("setObject", Array(classOf[AnyRef]))
      val suspend = cci.getMethod("suspend", Array(java.lang.Long.TYPE))
      val resume = cci.getMethod("resume", null)
      /* FIXME disable Jetty Support */
      (true && false, (cc), (meth), (getObj), (setObj), (suspend), resume)
    } catch {
      case e => (false, null, null, null, null, null, null)
    }
  }
  
  def resumeRequest(what: AnyRef, req: HttpServletRequest) {
    val cont = getContinuation.invoke(contSupport, Array(req, LiftServlet))
    setObject.invoke(cont, Array(what))
    resume.invoke(cont, null)
  }
  
  def doContinuation(req: HttpServletRequest): Nothing = {
    try {
      val cont = getContinuation.invoke(contSupport, Array(req, LiftServlet))
      Log.trace("About to suspend continuation")
      suspend.invoke(cont, Array(new java.lang.Long(200000L)))
      throw new Exception("Bail")
    } catch {
      case e: java.lang.reflect.InvocationTargetException if e.getCause.getClass.getName.endsWith("RetryRequest") => throw e.getCause
    }
  }
  
  def checkJetty(req: HttpServletRequest) = {
    if (!hasJetty_?) None
    else {
      val cont = getContinuation.invoke(contSupport, Array(req, LiftServlet))
      val ret = getObject.invoke(cont, null)
      setObject.invoke(cont, Array(null))
      ret
    }
  }
  
  private var _sitemap: Can[SiteMap] = Empty
  
  def setSiteMap(sm: SiteMap) {_sitemap = Full(sm)}
  def siteMap: Can[SiteMap] = _sitemap
  
  def appendEarly(f: HttpServletRequest => Any) = _early = _early ::: List(f)
  
  var ending = false
  private case class Never
  
  private def rpf[A,B](in: List[PartialFunction[A,B]], last: PartialFunction[A,B]): PartialFunction[A,B] = in match {
    case Nil => last
    case x :: xs => x orElse rpf(xs, last)
  }
  
  var statelessDispatchTable: DispatchPf = Map.empty
  
  def dispatchTable(req: HttpServletRequest): DispatchPf = {
    // test_boot
    req.getSession.getAttribute(SessionDispatchTableName) match {
      case null | Nil  => dispatchTable_i 
      case dt: List[S.DispatchHolder] => rpf(dt.map(_.dispatch), dispatchTable_i)
      case _ => dispatchTable_i
    }
  }
  
  def rewriteTable(req: HttpServletRequest): RewritePf = {
    // test_boot
    req.getSession.getAttribute(SessionRewriteTableName) match {
      case null | Nil => rewriteTable_i
      case rt: List[S.RewriteHolder] => rpf(rt.map(_.rewrite), rewriteTable_i)
      case _ => rewriteTable_i
    }
  }
  
  def snippetTable: SnippetPf = snippetTable_i
  
  def templateTable: TemplatePf = {
    S.sessionTemplater match {
      case Nil => templateTable_i
      case rt => rpf(rt.map(_.template), templateTable_i)
    }
  }
  
  var ajaxPath = "ajax_request"
  
  var cometPath = "comet_request"
  
  private var _context: ServletContext = _
  
  def context: ServletContext = synchronized {_context}
  
  def setContext(in: ServletContext): Unit =  synchronized {
    if (in ne _context) {
      // Helpers.setResourceFinder(in.getResource)
      _context = in
      //   performBoot(in)
    }
  }
  
  private var otherPackages: List[String] = Nil
  
  def buildPackage(end: String)  = synchronized (otherPackages.map(_+"."+end))
  
  def addToPackages(what: String) {synchronized {otherPackages = what :: otherPackages}}  
  
  private val defaultFinder = getClass.getResource _
  private def resourceFinder(name: String): java.net.URL = _context.getResource(name)
  
  def getResource(name: String): Can[java.net.URL] = resourceFinder(name) match {case null => defaultFinder(name) match {case null => Empty; case s => Full(s)} ; case s => Full(s)} 
  def getResourceAsStream(name: String): Can[java.io.InputStream] = getResource(name).map(_.openStream)
  def loadResource(name: String): Can[Array[Byte]] = getResourceAsStream(name).map{
    stream =>
    val buffer = new Array[Byte](2048)
    val out = new ByteArrayOutputStream
    def reader {
      val len = stream.read(buffer)
      if (len < 0) return
      else if (len > 0) out.write(buffer, 0, len)
      reader
    }
    reader
    stream.close
    out.toByteArray
  }
  def loadResourceAsXml(name: String): Can[NodeSeq] = loadResourceAsString(name).flatMap(s =>PCDataXmlParser(s))
  def loadResourceAsString(name: String): Can[String] = loadResource(name).map(s => new String(s, "UTF-8"))
  
  
  
  def finder(name: String): Can[InputStream] = {
    LiftServlet.context match {
      case null => Empty
      case c => c.getResourceAsStream(name) match {
        case null => Empty
        case s => Full(s)
      }
    }
  }  
  
  /**
  * Get the partial function that defines if a request should be handled by
  * the application (rather than the default servlet handler)
  */
  def isLiftRequest_? : LiftRequestPf = i_isLiftRequest_?
  
  /**
  * Append a partial function to the list of interceptors to test
  * if the request should be handled by lift
  */
  def addLiftRequest(what: LiftRequestPf) {i_isLiftRequest_? = i_isLiftRequest_? orElse what}
  
  private var i_isLiftRequest_? : LiftRequestPf = Map.empty
  
  private var dispatchTable_i : DispatchPf = Map.empty
  
  private var rewriteTable_i : RewritePf = Map.empty
  
  private var templateTable_i: TemplatePf = Map.empty
  
  private var snippetTable_i: SnippetPf = Map.empty
  
  var cometLoggerBuilder: () => LiftLogger = () => {
    val ret = LogBoot.loggerByName("comet_trace")
    ret.level = LiftLogLevels.Off
    ret
  }
  
  lazy val cometLogger: LiftLogger = cometLoggerBuilder()
  
  def addSnippetBefore(pf: SnippetPf) = {
    snippetTable_i = pf orElse snippetTable_i
    snippetTable_i
  }
  
  def addSnippetAfter(pf: SnippetPf) = {
    snippetTable_i = snippetTable_i orElse pf
    snippetTable_i
  }
  
  def addTemplateBefore(pf: TemplatePf) = {
    templateTable_i = pf orElse templateTable_i
    templateTable_i
  }
  
  def addTemplateAfter(pf: TemplatePf) = {
    templateTable_i = templateTable_i orElse pf
    templateTable_i
  }
  
  def addRewriteBefore(pf: RewritePf) = {
    rewriteTable_i = pf orElse rewriteTable_i
    rewriteTable_i
  }
  
  def addRewriteAfter(pf: RewritePf) = {
    rewriteTable_i = rewriteTable_i orElse pf
    rewriteTable_i
  }
  
  def addDispatchBefore(pf: DispatchPf) = {
    dispatchTable_i = pf orElse dispatchTable_i
    dispatchTable_i
  }
  
  def addDispatchAfter(pf: DispatchPf) = {
    dispatchTable_i = dispatchTable_i orElse pf
    dispatchTable_i
  }
  
  /**
  * Takes a Node, headers, cookies, and a session and turns it into an XhtmlResponse.
  */
  private def cvt(ns: Node, headers: List[(String, String)], cookies: List[Cookie], session: RequestState) = 
  convertResponse((XhtmlResponse(Group(session.fixHtml(ns)), 
  ResponseInfo.docType(session),
  headers, cookies, 200), headers, cookies, session))
  
  var defaultHeaders: PartialFunction[(NodeSeq, RequestState), List[(String, String)]] = {
    case _ => List(("Expires", "0"))
  }
  
  /**
  * convertResponse is a PartialFunction that reduces a given Tuple4 into a 
  * ResponseIt that can then be sent to the browser.
  */
  var convertResponse: PartialFunction[(Any, List[(String, String)], List[Cookie], RequestState), Response] = {
    case (r: ResponseIt, _, _, _) => r.toResponse
    case (ns: Group, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: Node, headers, cookies, session) => cvt(ns, headers, cookies, session)
    case (ns: NodeSeq, headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    case (ns: Seq[Node], headers, cookies, session) => cvt(Group(ns), headers, cookies, session)
    
    case (Full(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    
    case (Some(o), headers, cookies, session) => convertResponse( (o, headers, cookies, session) )
    case (bad, _, _, session) => session.createNotFound.toResponse
  }
  
  /**
  * Set a snippet failure handler here.  The class and method for the snippet are passed in
  */
  var snippetFailedFunc: List[SnippetFailure => Unit] = logSnippetFailure _ :: Nil
  
  private def logSnippetFailure(sf: SnippetFailure) = Log.warn("Snippet Failure: "+sf)
  
  case class SnippetFailure(page: String, typeName: Can[String], failure: SnippetFailures.Value)
  
  object SnippetFailures extends Enumeration {
    val NoTypeDefined = Value(1, "No Type Defined")
    val ClassNotFound = Value(2, "Class Not Found")
    val StatefulDispatchNotMatched = Value(3, "Stateful Snippet: Dispatch Not Matched")
    val MethodNotFound = Value(4, "Method Not Found")
    val NoNameSpecified = Value(5, "No Snippet Name Specified")
  }
  
  /**
  * The function that deals with how exceptions are presented to the user during processing
  * of an HTTP request.  Put a new function here to change the behavior.
  *
  * The function takes the RequestState and the Exception and returns a ResponseIt that's 
  * sent to the browser.
  */
  var logAndReturnExceptionToBrowser: (RequestState, Throwable) => ResponseIt = showException
  
  /**
  * The partial function (pattern matching) for handling converting an exception to something to
  * be sent to the browser depending on the current RunMode (development, etc.)
  *
  * The best thing to do is browserResponseToException = { case (...) => } orElse browserResponseToException
  * so that your response over-rides the default, but the processing falls through to the default.
  */
  var browserResponseToException: PartialFunction[(Props.RunModes.Value, RequestState, Throwable), ResponseIt] = {
    case (Props.RunModes.Development, r, e) =>
    XhtmlResponse((<html><body>Exception occured while processing {r.uri} 
    <pre>{
      _showException(e)     
    }</pre></body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html"), Nil, 500)
    
    case (_, r, e) => 
    XhtmlResponse((<html><body>Something unexpected happened while serving the page at {r.uri} 
    </body></html>),ResponseInfo.docType(r), List("Content-Type" -> "text/html"), Nil, 500)
  }
  
  /**
  * A utility method to convert an exception to a string of stack traces
  * @param le the exception
  *
  * @return the stack trace
  */
  def _showException(le: Throwable): String = {
    val ret = "Message: "+le.toString+"\n"+
    le.getStackTrace.map(_.toString).mkString("\n") + "\n"
    
    val also = le.getCause match {
      case null => ""
      case sub: Throwable => "Caught and thrown by:\n"+ _showException(sub)
    }
    
    ret + also
  }
  
  private def showException(r: RequestState, e: Throwable): ResponseIt = {
    Log.error("Exception being returned to browser when processing "+r, e)
    browserResponseToException(Props.mode, r, e)
  }
  
  
}

case object BreakOut

abstract class Bootable
{
  def boot() : Unit;
}

private[http] case object DefaultBootstrap extends Bootable
{
  def boot() : Unit =
  {
    val f = createInvoker("boot", Class.forName("bootstrap.liftweb.Boot").newInstance.asInstanceOf[AnyRef])
    f.map{f => f()}
  }
}

class LiftFilter extends Filter 
{
  //The variable holds the current ServletContext (we need it for request URI - handling
  private var context : ServletContext = null
  private var actualServlet: LiftServlet = _
  
  def doFilter(req: ServletRequest, res: ServletResponse,chain: FilterChain) =
  {      
    
    (req, res) match {
      case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
      LiftServlet.early.foreach(_(httpReq))
      val session = RequestState(httpReq, LiftServlet.rewriteTable(httpReq), System.nanoTime)
      if (isLiftRequest_?(session)) lift(httpReq, httpRes, session)
      else chain.doFilter(req, res)
      
      case _ => chain.doFilter(req, res)
    }
  }
  
  //We need to capture the ServletContext on init
  def init(config:FilterConfig) {
    context = config.getServletContext
    
    LiftServlet.setContext(context)       
    bootLift(Can(config.getInitParameter("bootloader")))
    
    actualServlet = new LiftServlet(context)
    actualServlet.init
  }
  
  //And throw it away on destruction
  def destroy {
    context = null
    if (actualServlet != null) {
      actualServlet.destroy
      actualServlet = null 
    }
  }
  
  def bootLift(loader : Can[String]) : Unit =
  {
    try 
    {        
      val b : Bootable = loader.map(b => Class.forName(b).newInstance.asInstanceOf[Bootable]) openOr DefaultBootstrap
      
      b.boot
      postBoot
    } catch {
      case e => Log.error("Failed to Boot", e); None
    }
  }
  
  private def postBoot {
    try {
      ResourceBundle getBundle (LiftServlet.liftCoreResourceName)
    } catch {
      case _ => Log.error("LiftWeb core resource bundle was not found ! ")
    }
  }
  
  private def lift(req: HttpServletRequest, res: HttpServletResponse, session: RequestState): Unit = 
  {
    actualServlet.service(req, res, session)
  }
  
  //This function tells you wether a resource exists or not, could probably be better
  private def liftHandled(in: String): Boolean = (in.indexOf(".") == -1) || in.endsWith(".html") || in.endsWith(".xhtml") ||
  in.endsWith(".htm") ||
  in.endsWith(".xml") || in.endsWith(".liftjs") || in.endsWith(".liftcss")
  
  private def isLiftRequest_?(session: RequestState): Boolean = {
    if (LiftServlet.isLiftRequest_?.isDefinedAt(session)) LiftServlet.isLiftRequest_?(session)
    else session.path.endSlash || (session.path.path.takeRight(1) match {case Nil => true case x :: xs => liftHandled(x)}) || 
    context.getResource(session.uri) == null
  }
}
