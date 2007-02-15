package net.liftweb.http;

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

 import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
 import scala.collection.immutable.{Map, ListMap}
 import scala.collection.mutable.{HashSet, HashMap}
 import java.net.URLDecoder
 import scala.xml.{Node, NodeSeq, Elem, MetaData, Null, UnprefixedAttribute, XML, Comment}
 import scala.xml.transform._
 import scala.actors._
 import scala.actors.Actor._
 import net.liftweb.util.Helpers._


 /**
  * An implementation of HttpServlet.  Just drop this puppy into 
  * your Java web container, do a little magic in web.xml, and
  * ta-da, you've got a scala-powered Servlet
  * 
  */
 class Servlet extends HttpServlet {
   private val actorNameConst = "the_actor"
     
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
     val in = getServletContext.getResourceAsStream(request.getRequestURI.substring(request.getContextPath.length))
     
     val li = request.getRequestURI.lastIndexOf('.')
     if (li != -1) {
       response.setContentType(request.getRequestURI.substring(li + 1) match {
         case "js" => "text/javascript"
         case "css" => "text/css"
         case "png" => "image/png"
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
       case _ => {val ret = new Session; ret.start; ret ! "Hello"; session.putValue(actorNameConst, ret); ret}
     }
   }
   
   override def service(req: HttpServletRequest,resp: HttpServletResponse) {
     Console.println("Servicing...")
     req.getMethod.toUpperCase match {
       case "GET" => doGet(req, resp)
       case _ => doService(req, resp, RequestType(req))
     }
   }
   
   /**
     * Service the HTTP request
     */ 
   def doService(request:HttpServletRequest , response: HttpServletResponse, requestType: RequestType ) : unit = {
     val session = RequestState(request, getServletContext.getResourceAsStream)
     
     val sessionActor = getActor(request.getSession)
     Console.println("Sending request")
     
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
     }
     
     // FIXME -- in Jetty, use continuations 
     sessionActor ! session
     
     val resp = receiveWithin(10000l) {
       case r @ Response(_,_,_) => Some(r)
       case Some(r : Response) => Some(r)
       case TIMEOUT => None
     } match {
       case Some(r: Response) => r
       case _ => {Console.println("Got a None back"); session.createNotFound}
     }
     
     val bytes = resp.out.toString.getBytes("UTF-8")
     val len = bytes.length
     // insure that certain header fields are set
     val header = insureField(resp.headers, List({"Content-Type", "text/html"},
                                                 {"Content-Encoding", "UTF-8"},
                                                 {"Content-Length", len.toString}));
     
     // send the response
     header.elements.foreach {n => response.setHeader(n._1, n._2)}
     response setStatus resp.code
     response.getOutputStream.write(bytes)
   }
 }
  

