package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http._
import javax.servlet.ServletContext
import scala.collection.Map
// import scala.collection.mutable.HashMap
import scala.collection.immutable.{TreeMap, SortedMap}
import net.liftweb.util.Helpers._
import net.liftweb.util.Lazy
import net.liftweb.sitemap._
import java.io.InputStream
import scala.xml._


object RequestState {
  object NilPath extends ParsePath(Nil, true, false)
  
  def apply(request: HttpServletRequest, rewrite: Servlet.rewritePf, context: ServletContext): RequestState = {
    val reqType = RequestType(request)
    val turi = request.getRequestURI.substring(request.getContextPath.length)
    val tmpUri = if (turi.length > 0) turi else "/"
    val contextPath = request.getContextPath
    val tmpPath = parsePath(tmpUri)

    def processRewrite(uri: String, path: ParsePath, params: SortedMap[String, String]): RewriteResponse = {
      val toMatch = RewriteRequest(uri, path, reqType, request)
      if (!rewrite.isDefinedAt(toMatch)) RewriteResponse(uri, path, params)
      else {
        //val (newUri, newPath, newMap) = rewrite(toMatch)
        //processRewrite(newUri, newPath, newMap)
        rewrite(toMatch)
      }
    }
    
    // val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    val rewritten = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    
    val localParams = TreeMap(rewritten.params.map(a => (a._1, List(a._2))).toList :_*)
    
    // val session = request.getSession
    val body = (if (reqType.post_? && request.getContentType == "text/xml") readWholeStream(request.getInputStream) else null)
      
      val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
    val tmp = paramNames.map{n => (n, request.getParameterValues(n).toList)}
    val params = localParams ++ paramNames.map{n => (n, request.getParameterValues(n).toList)}
    
    new RequestState(paramNames, params,rewritten.uri,rewritten.path,contextPath, reqType,/* resourceFinder,*/
		     rewritten.path.path.take(1) match {case List("rest") | List("soap") => true; case _ => false},
		     body, request.getContentType, request, context)
  }
  
  def nil = new RequestState(Nil, Map.empty, "", NilPath, "", GetRequest(false), false, null, "", null, null)
  
  def parsePath(in: String): ParsePath = {
    val p1 = (in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/")
    val front = p1.startsWith("/")
    val back = p1.length > 1 && p1.endsWith("/")
    ParsePath(p1.replaceAll("/$", "/index").split("/").toList.filter(_.length > 0), front, back)
  }
}

case class ParsePath(path: List[String], absolute: boolean, endSlash: boolean) {
  def drop(cnt: int) = ParsePath(path.drop(cnt), absolute, endSlash)
}


class RequestState(val paramNames: List[String],
		   val params: Map[String, List[String]],
		   val uri: String,val path: ParsePath,
		   val contextPath: String,
		   val requestType: RequestType,
		   val webServices_? : boolean,
		   val body: Array[byte],
                   val contentType: String,
                   val request: HttpServletRequest,
                   val context: ServletContext) 
{
  def xml_? = contentType != null && contentType.toLowerCase.startsWith("text/xml")
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}
  val id = pathParam(0)
  def pathParam(n: int) = head(path.path.drop(n + 2), "")
  def path(n: int) = head(path.path.drop(n), null)
  def param(n: String) = params.get(n) match {
    case Some(s :: _) => Some(s)
    case _ => None
  }
  
  private val _location = Lazy(() => Servlet.siteMap.flatMap(_.findLoc(this, request)))
  def location = _location.get 
  
  def testLocation: Option[RedirectWithMessage] = if (Servlet.siteMap.isEmpty) None
     else location.map(_.testAccess) getOrElse Some(RedirectWithMessage("/", "Invalid URL"))
  
  private val _buildMenu = Lazy(() => location.map(_.buildMenu) getOrElse CompleteMenu(Nil))
  
  def buildMenu: CompleteMenu = _buildMenu.get  

  def finder(name: String): Option[InputStream] = {
    context match {
      case null => None
      case c => c.getResourceAsStream(name) match {
        case null => None
        case s => Some(s)
      }
    }
  }
  
  def createNotFound = {
    XhtmlResponse(<html><body>The Requested URL {contextPath+this.uri} was not found on this server</body></html>, TreeMap.empty, 404)
  }
  
  def showException(e: Throwable) = {
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
    XhtmlResponse(<html><body>Exception occured while processing {this.uri} 
	     <pre>{
	       
	       et      
	     }</pre></body></html>, Map("Content-Type" -> "text/html"), 500)
  }
  
  def post_? = requestType.post_?
  def get_? = requestType.get_?
  def put_? = requestType.put_?
  def ajax_? = requestType.ajax_?
  
  def fixHtml(in : NodeSeq) : NodeSeq = {
    def fixHref(v : Seq[Node]) : String = {
      val hv = v.elements.next.text
      if (hv.startsWith("/")) contextPath+hv
      else hv
    }
    
    def fixAttrs(toFix : String, attrs : MetaData) : MetaData = {
      if (attrs == Null) Null else
	if (attrs.key == toFix) new UnprefixedAttribute(toFix,fixHref(attrs.value),fixAttrs(toFix, attrs.next))
	else attrs.copy(fixAttrs(toFix, attrs.next))
    }
    in.map{
      v => 
	v match {
	  case <form>{ _* }</form> => {Elem(v.prefix, v.label, fixAttrs("action", v.attributes), v.scope, fixHtml(v.child) : _* )}
          case <script>{ _* }</script> => {Elem(v.prefix, v.label, fixAttrs("src", v.attributes), v.scope, fixHtml(v.child) : _* )}
          case <img>{ _* }</img> => {Elem(v.prefix, v.label, fixAttrs("src", v.attributes), v.scope, fixHtml(v.child) : _* )}
	  case <a>{ _* }</a> => {Elem(v.prefix, v.label, fixAttrs("href", v.attributes), v.scope, fixHtml(v.child) : _* );}
	  case <link/> => {Elem(v.prefix, v.label, fixAttrs("href", v.attributes), v.scope, fixHtml(v.child) : _* )}
	  /*
	   case <input>{ _* }</input> | <textarea>{ _* }</textarea> => 
	   {v.attribute("name") match {case null => {} ; case s @ _ => {inputNames += s.elements.next.text}}; Elem(v.prefix, v.label, v.attributes, v.scope, fixHtml(v.child) : _*)}
	   */
	  case Elem(_,_,_,_,_*) => {Elem(v.prefix, v.label, v.attributes, v.scope, fixHtml(v.child) : _*)}
	  case _ => {v}
		   }
    }
  }
}

case class RequestMatcher(request: RequestState, path: ParsePath)
case class RewriteRequest(uri: String,path: ParsePath,requestType: RequestType,httpRequest: HttpServletRequest)
case class RewriteResponse(uri: String,path: ParsePath,params: SortedMap[String, String])
