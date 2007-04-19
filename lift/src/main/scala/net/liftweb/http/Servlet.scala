package net.liftweb.http;

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import javax.servlet.{ServletContext}
import scala.collection.immutable.{Map, ListMap}
import scala.collection.mutable.{HashSet, HashMap, ArrayBuffer}
import java.net.URLDecoder
import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment}
import scala.xml.transform._
import scala.actors._
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import java.io.InputStream
import net.liftweb.util.Helpers
import net.liftweb.util.ActorPing
import scala.collection.immutable.{TreeMap, SortedMap}

/**
 * An implementation of HttpServlet.  Just drop this puppy into 
 * your Java web container, do a little magic in web.xml, and
 * ta-da, you've got a scala-powered Servlet
 * 
 */
class Servlet extends HttpServlet {
  private val actorNameConst = "the_actor"
  private var requestCnt = 0
  private val selves = new ArrayBuffer[Actor]
  
  override def destroy = {
    try {
    Servlet.ending = true
    this.synchronized {
      while (requestCnt > 0) {
        selves.foreach(s => s ! None)
        wait(100L)
      }
    }
    Scheduler.snapshot // pause the Actor scheduler so we don't have threading issues
    ActorPing.snapshot
    Console.println("Destroyed servlet")
    super.destroy
    } catch {
      case e => e.printStackTrace
    }
  }
  
  override def init = {
    if (Scheduler.tasks ne null) {Console.println("Restarting Scheduler"); Scheduler.restart} // restart the Actor scheduler
    ActorPing.start
    Servlet.ending = false
    super.init
  }
  
  /**
   * Forward the GET request to the POST handler
   */
  override def doGet(request : HttpServletRequest , response : HttpServletResponse ) = {
    if (isExistingFile_?(request)) doServiceFile(request, response) else
      doService(request, response, RequestType(request ))
  }

  /**
   * Is the file an existing file in the WAR?
   */
  private def isExistingFile_?(request : HttpServletRequest) : boolean = {
    goodPath_?(request.getRequestURI) &&
    (getServletContext.getResource(request.getRequestURI.substring(request.getContextPath.length)) ne null)
  }
  
  private def doServiceFile(request : HttpServletRequest , response : HttpServletResponse) = {
    val in = getServletContext.getResourceAsStream(request.getRequestURI.substring(request.getContextPath.length).trim)
    
    val li = request.getRequestURI.lastIndexOf('.')
    if (li != -1) {
      response.setContentType(request.getRequestURI.substring(li + 1) match {
        case "js" => "text/javascript"
        case "css" => "text/css"
        case "png" => "image/png"
        case "gif" => "image/gif"
        case "ico" => "image/x-icon"
        case _ => "text/html"
      })
    }

    val out = response.getOutputStream
    val ba = new Array[byte](2048)

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
  }
  
  
  def getActor(session: HttpSession) = {
    session.getValue(actorNameConst) match {
      case r : Session => r
      case _ => 
        val ret = new Session
        ret.start
        session.putValue(actorNameConst, ret)
        ret
    }
  }
  
  override def service(req: HttpServletRequest,resp: HttpServletResponse) {

    printTime("Service request "+req.getRequestURI) {
    Servlet.setContext(getServletContext)
    req.getMethod.toUpperCase match {
      case "GET" => doGet(req, resp)
      case _ => doService(req, resp, RequestType(req))
    }
    }
  }
  
  /**
   * Service the HTTP request
   */ 
  def doService(request:HttpServletRequest , response: HttpServletResponse, requestType: RequestType ) : unit = {
    Servlet.early.foreach(_(request))
    val session = RequestState(request, Servlet.rewriteTable)
    
    val finder = &getServletContext.getResourceAsStream
    
    val toMatch = (session, session.path, finder)
      
      val resp: Response = if (Servlet.ending) {
        session.createNotFound
      } else if (Servlet.dispatchTable.isDefinedAt(toMatch)) {
	S.init(session) {
	  val f = Servlet.dispatchTable(toMatch)
	  f(request) match {
            case None => session.createNotFound
            case Some(v) => Servlet.convertResponse(v, session)
	  }
	}
      } else {
	
	val sessionActor = getActor(request.getSession)
	
	if (false) {
	  val he = request.getHeaderNames
	  while (he.hasMoreElements) {
	    val e = he.nextElement.asInstanceOf[String];
	    val hv = request.getHeaders(e)
	    while (hv.hasMoreElements) {
              val v = hv.nextElement
              Console.println(e+": "+v)
	    }
	  }
          Console.println(session.params)
	}
	
        try {
          this.synchronized {
            this.requestCnt = this.requestCnt + 1
            selves += self
          }
          
        def drainTheSwamp { // remove any message from the current thread's inbox
          receiveWithin(0) {
            case TIMEOUT => true
            case s @ _ => Console.println("Drained "+s) ; false
          } match {
            case false => drainTheSwamp
            case _ =>
          }
        }
        
        drainTheSwamp

	val timeout = (if (session.ajax_?) 100 else 10) * 1000L
	sessionActor ! AskSessionToRender(session, request, finder, timeout)
        receiveWithin(timeout) {
          case r @ Response(_, _, _) => r
	  case Some(r: Response) => r
	  case n => Console.println("Got unknown (Servlet) resp "+n); session.createNotFound
	}
        } finally {
          this.synchronized {
            this.requestCnt = this.requestCnt - 1
            selves -= self
            this.notifyAll
          }
        }
      }
    
    
    val bytes = resp.out.toString.getBytes("UTF-8")
    val len = bytes.length
    // insure that certain header fields are set
    val header = insureField(resp.headers, List(("Content-Type", "text/html"),
                                                ("Content-Encoding", "UTF-8"),
                                                ("Content-Length", len.toString)));
    
    // send the response
    header.elements.foreach {n => response.setHeader(n._1, n._2)}
    response setStatus resp.code
    response.getOutputStream.write(bytes)
  }
}

object Servlet {
  type dispatchPf = PartialFunction[(RequestState, List[String], (String) => InputStream), (HttpServletRequest) => Option[Any]];
  type rewritePf = PartialFunction[(String, List[String], RequestType, HttpServletRequest), (String, List[String], SortedMap[String, String])]
             
  private var _early: List[(HttpServletRequest) => Any] = Nil
  
  def early = {
    test_boot
    _early
  }
  
  def appendEarly(f: (HttpServletRequest) => Any) {
    _early = _early ::: List(f)
  }
  
  var ending = false
  private case class Never
  def dispatchTable = {
    test_boot
    dispatchTable_i
  }
  
  def rewriteTable = {
    test_boot
    rewriteTable_i
  }

  private var _context: ServletContext = _
  
  def context: ServletContext = synchronized {_context}
  
  def setContext(in: ServletContext): unit =  synchronized {
    if (in ne _context) {
      Helpers.setResourceFinder(in.getResource)
      _context = in
      }
  }
  
  private var dispatchTable_i : dispatchPf = Map.empty
  
  private var rewriteTable_i : rewritePf = Map.empty
  
  private val test_boot = {
    try {
      val c = Class.forName("bootstrap.liftweb.Boot")
      val i = c.newInstance
      val f = createInvoker("boot", i)
      f.map{f => f()}
      
    } catch {
      case e: Exception => e.printStackTrace; None
    }
  }

  def addRewriteBefore(pf: rewritePf) = {
    rewriteTable_i = pf orElse rewriteTable_i
    rewriteTable_i
  }
  
  def addRewriteAfter(pf: rewritePf) = {
    rewriteTable_i = rewriteTable_i orElse pf
    rewriteTable_i
  }
  
  def addDispatchBefore(pf: dispatchPf) = {
    dispatchTable_i = pf orElse dispatchTable_i
    dispatchTable_i
  }
  
  def addDispatchAfter(pf: dispatchPf) = {
    dispatchTable_i = dispatchTable_i orElse pf
    dispatchTable_i
  }
  
  def convertResponse(r: Any, session: RequestState): Response = {
    r match {
      case r: Response => r
      case Some(r: Response) => r
      case ns: NodeSeq => Response(session.fixHtml(ns), null, 200)
      case xml: XmlResponse => Response(xml.xml, Map("Content-Type" -> "text/xml"), 200)
      case _ => session.createNotFound
    }
  }
}

