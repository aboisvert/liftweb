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
import java.net.URLDecoder
import scala.xml.{Node, NodeSeq,Group, Elem, MetaData, Null, XML, Comment, Text}
import scala.xml.transform._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import net.liftweb.mapper.DB
import net.liftweb.util._
import net.liftweb.util.Helpers
import net.liftweb.util.ActorPing
import java.util.{Locale, ResourceBundle}
import java.net.URL
import js._
import javax.servlet._

/**
* An implementation of HttpServlet.  Just drop this puppy into
* your Java web container, do a little magic in web.xml, and
* ta-da, you've got a scala-powered Servlet
*
*/
private[http] class LiftServlet extends HttpServlet  {
  private var requestCnt = 0
  private var servletContext: ServletContext = null

  def this(ctx: ServletContext) = {
    this()
    this.servletContext = ctx
  }
  
  override def getServletContext: ServletContext = servletContext

  override def destroy = {
    try {
      LiftRules.ending = true
      LiftRules.runUnloadHooks()
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
  
  override def init = {
    try {
      // if (Scheduler.tasks ne null) {Log.error("Restarting Scheduler"); Scheduler.restart} // restart the Actor scheduler
      LiftRules.ending = false
      LiftRules.addDispatchAfter({
        case RequestMatcher(r, ParsePath(mainPath :: subPath, _,_),_, _) if mainPath == LiftRules.ResourceServerPath => ResourceServer.findResourceInClasspath(r, subPath)
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
    val ret = session.getAttribute(LiftRules.sessionNameConst) match {
      case r: LiftSession => r
      case _ =>
      val ret = LiftSession(session, request.contextPath)
      session.setAttribute(LiftRules.sessionNameConst, ret)
      ret
    }
    ret.breakOutComet()
    ret
  }
  
  def service(req: HttpServletRequest,resp: HttpServletResponse, requestState: RequestState): Boolean = {
    try {
      def doIt: Boolean = {
        logTime("Service request ("+req.getMethod+") "+req.getRequestURI) {
          doService(req, resp, requestState)
        }
      }
      LiftRules.checkJetty(req) match {
        case None => doIt
        case r if r eq null => doIt
        case r: ResponseIt => sendResponse(r.toResponse, resp, Empty) ; true
        case Some(r: ResponseIt) => sendResponse(r.toResponse, resp, Empty); true
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
  def doService(request: HttpServletRequest, response: HttpServletResponse, requestState: RequestState): Boolean = {
    val statelessToMatch = RequestMatcher(requestState, requestState.path, RequestType(request), Empty)
    
    val resp: Can[Response] = if (LiftRules.ending) {
      LiftRules.notFoundOrIgnore(requestState, Empty)
    } else if (LiftRules.statelessDispatchTable.isDefinedAt(statelessToMatch)) {
      val f = LiftRules.statelessDispatchTable(statelessToMatch)
      f(requestState) match {
        case Full(v) => Full(LiftRules.convertResponse( (v, Nil, S.responseCookies, requestState) ))
        case Empty => LiftRules.notFoundOrIgnore(requestState, Empty)
        case f: Failure => Full(requestState.createNotFound(f).toResponse) 
      }
    } else {
      val sessionActor = getActor(requestState, request.getSession)
      val toMatch = RequestMatcher(requestState, requestState.path, RequestType(request), Full(sessionActor))
      
      val dispatch: (Boolean, Can[Response]) = S.init(requestState, sessionActor.notices, sessionActor) {
        if (LiftRules.dispatchTable(request).isDefinedAt(toMatch)) {
          try {
            val f = LiftRules.dispatchTable(request)(toMatch)
            f(requestState) match {
              case Full(v) => (true, Full(LiftRules.convertResponse( (sessionActor.checkRedirect(v), Nil, S.responseCookies, requestState) )))
              case Empty => (true, LiftRules.notFoundOrIgnore(requestState, Full(sessionActor)))
              case f: Failure => (true, Full(sessionActor.checkRedirect(requestState.createNotFound(f)).toResponse))
            }
          } finally {
            sessionActor.notices = S.getNotices
          }
        } else (false, Empty)
      }
      
      if (dispatch._1) dispatch._2 
      else if (requestState.path.path.length == 1 && requestState.path.path.head == LiftRules.cometPath) {
        
        LiftRules.cometLogger.debug("Comet Request: "+sessionActor.uniqueId+" "+requestState.params)
        
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
            
            if (actors.isEmpty) Full(new JsCommands(JsCmds.RedirectTo(LiftRules.noCometSessionPage) :: Nil).toResponse)
            else {
              
              actors.foreach{case (act, when) => act ! Listen(when)}
              
              val ret = drainTheSwamp((LiftRules.ajaxRequestTimeout openOr 120) * 1000L, Nil) 
              
              actors.foreach{case (act, _) => act ! Unlisten}
              
              val ret2 = drainTheSwamp(100L, ret)
              
              val jsUpdateTime = ret2.map(ar => "lift_toWatch['"+ar.who.uniqueId+"'] = '"+ar.when+"';").mkString("\n")
              val jsUpdateStuff = ret2.map(ar => ar.response.toJavaScript(sessionActor, ar.displayAll))
              
              val all = jsUpdateStuff.reverse.foldLeft(JsCmds.Noop)(_ & _) & JE.JsRaw(jsUpdateTime).cmd
              
              
              LiftRules.cometLogger.debug("Comet Request: "+sessionActor.uniqueId+" response: "+all.toJsCmd)
              
              Full((new JsCommands(JsCmds.Run(jsUpdateTime) :: jsUpdateStuff)).toResponse)
            }
          }
        } finally {
          sessionActor.exitComet(self)
        }
      } else if (requestState.path.path.length == 1 && requestState.path.path.head == LiftRules.ajaxPath) {
        LiftRules.cometLogger.debug("AJAX Request: "+sessionActor.uniqueId+" "+requestState.params)
        S.init(requestState, sessionActor) {
          try {
            val what = flatten(sessionActor.runParams(requestState))
            
            val what2 = what.flatMap{case js: JsCmd => List(js); case n: NodeSeq => List(n) case js: JsCommands => List(js)  case r: ResponseIt => List(r); case s => Nil}
            
            val ret = what2 match {
              case (n: Node) :: _ => XmlResponse(n).toResponse
              case (ns: NodeSeq) :: _ => XmlResponse(Group(ns)).toResponse
              case (r: ResponseIt) :: _ => r.toResponse
              case (js: JsCmd) :: xs  => (JsCommands(S.noticesToJsCmd::Nil) & ((js :: xs).flatMap{case js: JsCmd => List(js) case _ => Nil}.reverse)).toResponse
              case _ => (new JsCommands(JsCmds.Noop :: Nil)).toResponse
            }
            
            LiftRules.cometLogger.debug("AJAX Response: "+sessionActor.uniqueId+" "+ret)
            Full(ret)
          } finally {
            sessionActor.updateFunctionMap(S.functionMap)
          }
        }
      } else {
        try {
          this.synchronized {
            this.requestCnt = this.requestCnt + 1
          }
          
          sessionActor.processRequest(requestState).map(_.toResponse) // .what.toResponse
          
        } finally {
          this.synchronized {
            this.requestCnt = this.requestCnt - 1
            this.notifyAll
          }
        }
      }
    }
    
    resp match {
      case Full(resp) =>
      logIfDump(requestState, resp)
      
      sendResponse(resp, response, Full(requestState))
      true
      case _ => false
    }
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
    
    def fixHeaders(headers : List[(String, String)]) = headers map ((v) => v match {
      case ("Location", uri) => (v._1, request map (_ updateWithContextPath(uri)) openOr uri)
      case _ => v
    })
    
    val bytes = resp.data
    val len = bytes.length
    // insure that certain header fields are set
    val header = insureField(fixHeaders(resp.headers), List(("Content-Type", LiftRules.determineContentType(request)),
    ("Content-Encoding", "UTF-8"),
    ("Content-Length", len.toString)))
    
    LiftRules._beforeSend.foreach(_(resp, response, header, request))
    // set the cookies
    resp.cookies.foreach(cookie => response.addCookie(cookie))
    
    // send the response
    header.elements.foreach {case (name, value) => response.setHeader(name, value)}
    response setStatus resp.code
    response.getOutputStream.write(bytes)    
    LiftRules._afterSend.foreach(_(resp, response, header, request))
  }
  
  /**
  * Remove any thread-local associations
  */
  def clearThread: Unit = {
    // uncomment for Scala 2.6.1 to avoid memory leak
    Actor.clearSelf
    DB.clearThread
  }
  
}

class LiftFilter extends Filter
{
  //The variable holds the current ServletContext (we need it for request URI - handling
  private var context: ServletContext = null
  private var actualServlet: LiftServlet = _
  
  def doFilter(req: ServletRequest, res: ServletResponse,chain: FilterChain) =
  {
    
    (req, res) match {
      case (httpReq: HttpServletRequest, httpRes: HttpServletResponse) =>
      LiftRules.early.foreach(_(httpReq))
      
      val session = RequestState(httpReq, LiftRules.rewriteTable(httpReq), System.nanoTime)
      
      if (isLiftRequest_?(session) && actualServlet.service(httpReq, httpRes, session)) {}
      else chain.doFilter(req, res)
      
      case _ => chain.doFilter(req, res)
    }
  }
  
  //We need to capture the ServletContext on init
  def init(config:FilterConfig) {
    context = config.getServletContext
    
    LiftRules.setContext(context)       
    bootLift(Can.legacyNullTest(config.getInitParameter("bootloader")))
    
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
      ResourceBundle getBundle (LiftRules.liftCoreResourceName)
    } catch {
      case _ => Log.error("LiftWeb core resource bundle for locale " + Locale.getDefault() + ", was not found ! ")
    }
  }
  
  
  //This function tells you wether a resource exists or not, could probably be better
  private def liftHandled(in: String): Boolean = (in.indexOf(".") == -1) || in.endsWith(".html") || in.endsWith(".xhtml") ||
  in.endsWith(".htm") ||
  in.endsWith(".xml") || in.endsWith(".liftjs") || in.endsWith(".liftcss")
  
  private def isLiftRequest_?(session: RequestState): Boolean = {
    if (LiftRules.isLiftRequest_?.isDefinedAt(session)) LiftRules.isLiftRequest_?(session)
    else session.path.endSlash || (session.path.path.takeRight(1) match {case Nil => true case x :: xs => liftHandled(x)}) ||
    context.getResource(session.uri) == null
  }
  
}

