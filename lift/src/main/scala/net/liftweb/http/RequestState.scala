package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http._
import javax.servlet.ServletContext
// import scala.collection.Map
// import scala.collection.mutable.HashMap
import net.liftweb.util.Helpers._
import net.liftweb.util.{Log, Can, Full, Empty, Failure}
import net.liftweb.sitemap._
import java.io.InputStream
import scala.xml._


object RequestState {
  object NilPath extends ParsePath(Nil, true, false)
  
  def apply(request: HttpServletRequest, rewrite: LiftServlet.RewritePf, nanoStart: Long): RequestState = {
    val reqType = RequestType(request)
    val turi = request.getRequestURI.substring(request.getContextPath.length)
    val tmpUri = if (turi.length > 0) turi else "/"
    val contextPath = request.getContextPath
    val tmpPath = parsePath(tmpUri)

    def processRewrite(uri: String, path: ParsePath, params: Map[String, String]): RewriteResponse = {
      val toMatch = RewriteRequest(uri, path, reqType, request)
      if (!rewrite.isDefinedAt(toMatch)) RewriteResponse(uri, path, params)
      else {
        //val (newUri, newPath, newMap) = rewrite(toMatch)
        //processRewrite(newUri, newPath, newMap)
        rewrite(toMatch)
      }
    }
    
    /*
    def xlateIfGet(in: List[String]): List[String] = {
      if (!reqType.get_? || !Servlet.getXLator.isDefined) in
      else {
        val xl = Servlet.getXLator.get
        in.map(s => xl(s))
      }
    }*/
    
    // val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    val rewritten = processRewrite(tmpUri, tmpPath, Map.empty)
    
    val localParams: Map[String, List[String]] = Map(rewritten.params.toList.map{case (name, value) => name -> List(value)} :_*)

    // val session = request.getSession
   //  val body = ()
   val eMap = Map.empty[String, List[String]]
   
    val (paramNames, params: Map[String, List[String]], body) = if (reqType.post_? && request.getContentType == "text/xml") {
      (Nil,localParams, readWholeStream(request.getInputStream))
    } else if (reqType.get_?) {
        request.getQueryString match {
          case null => (Nil, localParams, null)
          case s =>
          val pairs = s.split("&").toList.map(_.trim).filter(_.length > 0).map(_.split("=").toList match {
            case name :: value :: Nil => (true, urlDecode(name), urlDecode(value))
            case name :: Nil => (true, urlDecode(name), "")
            case _ => (false, "", "")
          }).filter(_._1).map{case (_, name, value) => (name, value)}
          val names = pairs.map(_._1).removeDuplicates
          val params = pairs.foldLeft(eMap) (
            (a,b) => a.get(b._1) match {
              case None => a + b._1 -> List(b._2)
              case Some(xs) => a + b._1 -> (xs ::: List(b._2))
            }
          )
          
          val hereParams = localParams ++ params
          
          (names, hereParams, null)
        }
    } else {
      val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
    // val tmp = paramNames.map{n => (n, xlateIfGet(request.getParameterValues(n).toList))}
    val params = localParams ++ paramNames.map{n => (n, request.getParameterValues(n).toList)}
      (paramNames, params, null)
    }
    
    new RequestState(paramNames, params,rewritten.uri,rewritten.path,contextPath, reqType,/* resourceFinder,*/
		     rewritten.path.path.take(1) match {case List("rest") | List("soap") => true; case _ => false},
		     body, request.getContentType, request, nanoStart, System.nanoTime)
  }
  
  def nil = new RequestState(Nil, Map.empty, "", NilPath, "", GetRequest, false, null, "", null, System.nanoTime, System.nanoTime)
  
  def parsePath(in: String): ParsePath = {
    val p1 = (in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/")
    val front = p1.startsWith("/")
    val back = p1.length > 1 && p1.endsWith("/")
    ParsePath(p1.replaceAll("/$", "/index").split("/").toList.filter(_.length > 0), front, back)
  }
  
  var fixHref = _fixHref _
  
  private def _fixHref(contextPath: String, v : Seq[Node]) : String = {
    val hv = v.text
    if (hv.startsWith("/")) contextPath+hv
    else hv
  }
  
  def fixHtml(contextPath: String, in : NodeSeq) : NodeSeq = {
    if (contextPath.length == 0) in
    else {
    def fixAttrs(toFix : String, attrs : MetaData) : MetaData = {
      if (attrs == Null) Null 
      else if (attrs.key == toFix) new UnprefixedAttribute(toFix,RequestState.fixHref(contextPath, attrs.value),fixAttrs(toFix, attrs.next))
      else attrs.copy(fixAttrs(toFix, attrs.next))
    }

    in.map{
      v => 
        v match {
          case Group(nodes) => Group(fixHtml(contextPath, nodes))

          case (<form>{ _* }</form>) => Elem(v.prefix, v.label, fixAttrs("action", v.attributes), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<script>{ _* }</script>) => Elem(v.prefix, v.label, fixAttrs("src", v.attributes), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<img>{ _* }</img>) => Elem(v.prefix, v.label, fixAttrs("src", v.attributes), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<a>{ _* }</a>) => Elem(v.prefix, v.label, fixAttrs("href", v.attributes), v.scope, fixHtml(contextPath, v.child) : _* )
          case (<link/>) => Elem(v.prefix, v.label, fixAttrs("href", v.attributes), v.scope, fixHtml(contextPath, v.child) : _* )
          case Elem(_,_,_,_,_*) => Elem(v.prefix, v.label, v.attributes, v.scope, fixHtml(contextPath, v.child) : _*)
          case _ => v
       }
    }
    }
  }  
}

case class ParsePath(path: List[String], absolute: Boolean, endSlash: Boolean) {
  def drop(cnt: int) = ParsePath(path.drop(cnt), absolute, endSlash)
}

class RequestState(val paramNames: List[String], val params: Map[String, List[String]], val uri: String,
		           val path: ParsePath,
		           val contextPath: String,
		           val requestType: RequestType,
		           val webServices_? : Boolean,
		           val body: Array[Byte],
                   val contentType: String,
                   val request: HttpServletRequest,
                   val nanoStart: Long,
                   val nanoEnd: Long) 
{
  def xml_? = contentType != null && contentType.toLowerCase.startsWith("text/xml")
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}
  val id = pathParam(0)
  def pathParam(n: int) = head(path.path.drop(n + 2), "")
  def path(n: int):String = head(path.path.drop(n), null)
  def param(n: String) = params.get(n) match {
    case Some(s :: _) => Some(s)
    case _ => None
  }
  
  def xml: Can[Elem] = if (!xml_?) Empty
  else {
    try {
    Full(XML.load(new java.io.ByteArrayInputStream(this.body)))
    } catch {
      case e => Failure(e.getMessage, Full(e), Nil)
    }
  }
  
  lazy val location = LiftServlet.siteMap.flatMap(_.findLoc(this, request))
  
  def testLocation: Can[RedirectWithMessage] = if (LiftServlet.siteMap.isEmpty) Empty
     else location.map(_.testAccess) openOr Full(RedirectWithMessage("/", "Invalid URL"))
  

  lazy val buildMenu: CompleteMenu = location.map(_.buildMenu) openOr CompleteMenu(Nil)

  
  def createNotFound = {
    XhtmlResponse((<html><body>The Requested URL {contextPath+this.uri} was not found on this server</body></html>),
        ResponseInfo.xhtmlTransitional , Nil, 404)
  }
  
  def createNotFound(failure: Failure) = { // FIXME do failure stuff
    XhtmlResponse((<html><body>The Requested URL {contextPath+this.uri} was not found on this server</body></html>),
        ResponseInfo.xhtmlTransitional , Nil, 404)
  }
  
  def showException(e: Throwable) = {
    Log.error("Exception being returned to browser", e)
    def _showException(le: Throwable): String = {
      val ret = "Message: "+le.toString+"\n"+
      le.getStackTrace.map{st => st.toString}.mkString("","\n","") + "\n"
      
      val also = le.getCause match {
	case sub: Throwable => "Caught and thrown by:\n"+ _showException(sub)
	case _ => ""
      }
      
      ret + also
    }
    val et = _showException(e)
    XhtmlResponse((<html><body>Exception occured while processing {this.uri} 
	     <pre>{
	       
	       et      
	     }</pre></body></html>),ResponseInfo.xhtmlTransitional, List("Content-Type" -> "text/html"), 500)
  }
  
  def post_? = requestType.post_?
  def get_? = requestType.get_?
  def put_? = requestType.put_?
  
  def fixHtml(in: NodeSeq): NodeSeq = RequestState.fixHtml(contextPath, in)
  
  def updateWithContextPath(uri: String): String = if (uri.startsWith("/")) contextPath + uri else uri
  
}

case class RequestMatcher(request: RequestState, path: ParsePath, session: LiftSession)
case class RewriteRequest(uri: String,path: ParsePath,requestType: RequestType,httpRequest: HttpServletRequest)
case class RewriteResponse(uri: String,path: ParsePath,params: Map[String, String])
