package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */
   
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse}
import scala.collection.Map
// import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap
import net.liftweb.util.Helpers._
import java.io.InputStream

object RequestState {
  def apply(request: HttpServletRequest, resourceFinder: (String) =>  InputStream): RequestState = {
    val session = request.getSession

    val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
    val tmp = paramNames.map{n => {n, request.getParameterValues(n).toList}}
    val params = TreeMap.Empty[String, List[String]] ++ paramNames.map{n => {n, request.getParameterValues(n).toList}}
    val uri = request.getRequestURI.substring(request.getContextPath.length)
    val contextPath = request.getContextPath
    val path = uri.split("/").toList.filter{n => n != null && n.length > 0}
    
    new RequestState(paramNames, params,uri,path,contextPath, RequestType(request), resourceFinder,
        path.take(1) match {case List("rest") | List("soap") => true; case _ => false})
  }
}

class RequestState(val paramNames: List[String],val params: Map[String, List[String]],
    val uri: String,val path: List[String],
    val contextPath: String,
    val requestType: RequestType,
    val resourceFinder: (String) =>  InputStream,
    val webServices_? : boolean) {
  
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}
  val id = pathParam(0)
  def pathParam(n: int) = head(path.drop(n + 2), "")
  def path(n: int) = head(path.drop(n), null)
  
  def createNotFound = {
    Response(<html><body>The Requested URL {this.uri} was not found on this server</body></html>, TreeMap.Empty, 404)
  }
  
  def post_? = requestType.post_?
  def get_? = requestType.get_?
  def put_? = requestType.put_?
  def ajax_? = requestType.ajax_?
}
