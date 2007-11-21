package net.liftweb.http;

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import javax.servlet.{ServletContext}
import scala.collection.mutable.{ListBuffer}
import java.net.URLDecoder
import scala.xml.{Node, NodeSeq,Group, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment}
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
import java.util.Locale

/**
 * An implementation of HttpServlet.  Just drop this puppy into 
 * your Java web container, do a little magic in web.xml, and
 * ta-da, you've got a scala-powered Servlet
 * 
 */
private[http] class LiftServlet(val getServletContext: ServletContext) extends AnyRef /* HttpServlet */ {
  private val actorNameConst = "the_actor"
  private var requestCnt = 0
  private val selves = new ListBuffer[Actor]
  
  def destroy = {
    try {
    LiftServlet.ending = true
    this.synchronized {
      while (requestCnt > 0) {
        selves.foreach(s => s ! None)
        wait(100L)
      }
    }
    Scheduler.snapshot // pause the Actor scheduler so we don't have threading issues
    ActorPing.snapshot
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
      if (Scheduler.tasks ne null) {Log.error("Restarting Scheduler"); Scheduler.restart} // restart the Actor scheduler
    ActorPing.start
    LiftServlet.ending = false
    // super.init
    } finally {
      clearThread
    }
  }
  
  /**
   * Forward the GET request to the POST handler
   */
     /*
  def doGet(request : HttpServletRequest , response : HttpServletResponse, start: Long) = {
    isExistingFile_?(request).map(u => doServiceFile(u, request, response)) openOr
      doService(request, response, RequestType(request ))
  }*/

  /**
   * Is the file an existing file in the WAR?
   */
  private def isExistingFile_?(request : HttpServletRequest) : Can[URL] = {
    if (!goodPath_?(request.getRequestURI)) Empty else
    getServletContext.getResource(request.getRequestURI.substring(request.getContextPath.length)) match {
      case null => Empty
      case u : URL => Full(u)
      case _ => Empty
    }
  }
  
  private def doServiceFile(url: URL, request : HttpServletRequest , response : HttpServletResponse) {
    val uc = url.openConnection
    val mod = request.getHeader("if-modified-since")
    if (mod != null) {
      val md = parseInternetDate(mod)
      if (uc.getLastModified <= md.getTime) {
        response.sendError(304)
        return
      }
    }
    val in = uc.getInputStream
    
    try {
    val li = request.getRequestURI.lastIndexOf('.')
    if (li != -1) {
      response.setContentType(request.getRequestURI.substring(li + 1) match {
        case "js" => "text/javascript"
        case "css" => "text/css"
        case "png" => "image/png"
        case "gif" => "image/gif"
        case "ico" => "image/x-icon"
        case _ => LiftServlet.determineContentType(request)
      })
    }
    
    response.setDateHeader("Last-Modified", uc.getLastModified)

    val out = response.getOutputStream
    val ba = new Array[Byte](2048)

    def readAndWrite {
      val len = in.read(ba)
      if (len >= 0) {
        if (len > 0) {
          out.write(ba, 0, len)
        }
        readAndWrite
      }
    }
    
    // a "while-less" read and write loop
    readAndWrite
    
    out.flush
    } finally {
      in.close
    }
  }
  
  
  def getActor(request: RequestState, session: HttpSession): LiftSession = {
    val ret = session.getValue(actorNameConst) match {
      case r: LiftSession => r
      case _ => 
        val ret = LiftSession(session, request.uri, request.path, request.contextPath, request.requestType, request.webServices_?,
            request.contentType)
        ret.start
        session.putValue(actorNameConst, ret)
        ret
    }
    ret.breakOutComet()
    ret
  }
  
  /* override */ def service(req: HttpServletRequest,resp: HttpServletResponse, session: RequestState) {
    try {
    def doIt {
      logTime("Service request "+req.getRequestURI) {
        LiftServlet.early.foreach(_(req))
        // FIXME need to do for normal servlet Servlet.setContext(getServletContext)
        doService(req, resp, session)
        /*
        req.getMethod.toUpperCase match {
          case "GET" => doGet(req, resp, start)
          case _ => doService(req, resp, RequestType(req), start)
        }*/
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
  def doService(request:HttpServletRequest , response: HttpServletResponse, session: RequestState) {
    

    val sessionActor = getActor(session, request.getSession)    
    val toMatch = RequestMatcher(session, session.path, sessionActor)
    
      val resp: Response = if (LiftServlet.ending) {
        session.createNotFound.toResponse
      } else if (LiftServlet.dispatchTable(request).isDefinedAt(toMatch)) {
        
         
	S.init(session, sessionActor) {
	  val f = LiftServlet.dispatchTable(request)(toMatch)
	  f(request) match {
            case Full(v) => LiftServlet.convertResponse(v, session)
            case Empty => session.createNotFound.toResponse
            case f: Failure => session.createNotFound(f).toResponse 
	  }
	}
      } else if (session.path.path.length == 1 && session.path.path.head == LiftServlet.cometPath) {
        sessionActor.enterComet(self)
        try {
        S.init(session, sessionActor) {
        val actors: List[(CometActor, Long)] = session.params.toList.flatMap{case (name, when) => sessionActor.getAsyncComponent(name).toList.map(c => (c, toLong(when)))}
        
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

        (new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff)).toResponse
        }
        }
        } finally {
          sessionActor.exitComet(self)
        }
        // Response("".getBytes("UTF-8"), Nil, 200)
      } else if (session.path.path.length == 1 && session.path.path.head == LiftServlet.ajaxPath) {
        S.init(session, sessionActor) {
            val what = flatten(sessionActor.runParams(session))
            val what2 = what.flatMap{case js: JsCmd => List(js); case n: NodeSeq => List(n) case js: JsCommands => List(js)  case r: ResponseIt => List(r); case s => Nil}

            what2 match {
              case (n: Node) :: _ => XmlResponse(n).toResponse
              case (ns: NodeSeq) :: _ => XmlResponse(Group(ns)).toResponse
              case (r: ResponseIt) :: _ => r.toResponse
              case (js: JsCmd) :: xs  => (new JsCommands((js :: xs).flatMap{case js: JsCmd => List(js) case _ => Nil}.reverse)).toResponse
              case _ => (new JsCommands(JsCmds.Noop :: Nil)).toResponse
            }
        }        
  } else {
        try {
          this.synchronized {
            this.requestCnt = this.requestCnt + 1
            self.trapExit = true
            selves += self
          }
          
        def drainTheSwamp { // remove any message from the current thread's inbox
          receiveWithin(0) {
            case TIMEOUT => true
            case s @ _ => Log.trace("Drained "+s) ; false
          } match {
            case false => drainTheSwamp
            case _ =>
          }
        }
        
        drainTheSwamp
        
        val timeout = (LiftServlet.calcRequestTimeout.map(_(session)) openOr (/*if (session.ajax_?) (Servlet.ajaxRequestTimeout openOr 120) 
            else*/ (LiftServlet.stdRequestTimeout openOr 10))) * 1000L        
        
        /*if (session.ajax_? && Servlet.hasJetty_?) {
          sessionActor ! AskSessionToRender(session, request, 120000L, a => Servlet.resumeRequest(a, request))
          Servlet.doContinuation(request)
        } else*/ {
	
        val thisSelf = self
	sessionActor ! AskSessionToRender(session, request, timeout, a => thisSelf ! a)
        receiveWithin(timeout) {
          case AnswerHolder(r) => r.toResponse
          // if we failed allow the optional handler to process a request 
	  case n @ TIMEOUT => LiftServlet.requestTimedOut.flatMap(_(session, n)) match {
            case Full(r) => r
            case _ => Log.warn("Got unknown (Servlet) resp "+n); session.createNotFound.toResponse
          }
	}
        }
        } finally {
          this.synchronized {
            this.requestCnt = this.requestCnt - 1
            selves -= self
            this.notifyAll
          }
        }
      }
    logIfDump(session, resp)
    
    sendResponse(resp, response, Full(session))
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
  
  def sendResponse(resp: Response, response: HttpServletResponse, request: Can[RequestState]) {
    val bytes = resp.data
    val len = bytes.length
    // insure that certain header fields are set
    val header = insureField(resp.headers, List(("Content-Type", LiftServlet.determineContentType(request)),
                                                ("Content-Encoding", "UTF-8"),
                                                ("Content-Length", len.toString)));
    
    LiftServlet._beforeSend.foreach(_(resp, response, header, request))
    
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

  type DispatchPf = PartialFunction[RequestMatcher, HttpServletRequest => Can[ResponseIt]];
  type RewritePf = PartialFunction[RewriteRequest, RewriteResponse]
  type TemplatePf = PartialFunction[RequestMatcher,() => Can[NodeSeq]]
  type SnippetPf = PartialFunction[List[String], NodeSeq => NodeSeq]
  
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

  def determineContentType(request: HttpServletRequest) : String = {
    request match {
      case null => "text/html"
      case request => determineContentType(request.getHeader("Accept"))
    }
  }

  def determineContentType(accept: String) : String = {
    // If application/xhtml+xml is explicitly listed then let's use that.
    // TODO(stevej): convert this into a match somehow. (ask david)
    if (accept != null && accept.contains("application/xhtml+xml")) {
      "application/xhtml+xml"
    } else {
      "text/html"
    }
  }

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
  var localeCalculator: Can[HttpServletRequest] => Locale = defaultLocaleCalculator _
  
  def defaultLocaleCalculator(request: Can[HttpServletRequest]) = request.flatMap(_.getLocale() match {case l: Locale => Full(l) case _ => Empty}).openOr(Locale.getDefault())

  
  val (hasJetty_?, contSupport, getContinuation, getObject, setObject, suspend, resume) = {
    try {
    val cc = Class.forName("org.mortbay.util.ajax.ContinuationSupport")
    val meth = cc.getMethod("getContinuation", Array(classOf[HttpServletRequest], classOf[AnyRef]))
    val cci = Class.forName("org.mortbay.util.ajax.Continuation")
    val getObj = cci.getMethod("getObject", null)
    val setObj = cci.getMethod("setObject", Array(classOf[AnyRef]))
    val suspend = cci.getMethod("suspend", Array(java.lang.Long.TYPE))
    val resume = cci.getMethod("resume", null)
    (true && false /* FIXME disable Jetty Support */, (cc), (meth), (getObj), (setObj), (suspend), resume)
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
      performBoot(in)
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
  
  private def performBoot(context: ServletContext) = {
    try {
      val c = Class.forName("bootstrap.liftweb.Boot")
      val i = c.newInstance
      val f = createInvoker("boot", i)
      f.map{f => f()}
      
    } catch {
    case e: java.lang.reflect.InvocationTargetException => Log.error("Failed to Boot", e); None
    case e => Log.error("Failed to Boot", e); None
    }
  }
  
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
  
  def convertResponse(r: Any, session: RequestState): Response = {
    r match {
      case r: ResponseIt => r.toResponse
      case ns: NodeSeq => convertResponse(XhtmlResponse(Group(session.fixHtml(Group(ns))), ResponseInfo.xhtmlTransitional, Nil, 200), session)
      case Some(o) => convertResponse(o, session)
      case _ => session.createNotFound.toResponse
    }
  }
}

case object BreakOut

class LiftFilter extends Filter 
{
  //The variable holds the current ServletContext (we need it for request URI - handling
   private var context : ServletContext = null
   private var actualServlet: LiftServlet = _
   
    def doFilter(req: ServletRequest, res: ServletResponse,chain: FilterChain) =
    {      
    
      (req, res) match {
        case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
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
      actualServlet = new LiftServlet(context)
      actualServlet.init
    }
    
    //And throw it away on destruction
    def destroy {
      context = null
      actualServlet.destroy
      actualServlet = null 
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
