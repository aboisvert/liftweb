package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse}
import scala.collection.Map
// import scala.collection.mutable.HashMap
import scala.collection.immutable.{TreeMap, SortedMap}
import net.liftweb.util.Helpers._
import java.io.InputStream
import scala.xml._

object RequestState {
  def apply(request: HttpServletRequest, rewrite: Servlet.rewritePf): RequestState = {
    val reqType = RequestType(request)
    val turi = request.getRequestURI.substring(request.getContextPath.length)
    val tmpUri = if (turi.length > 0) turi else "/"
    val contextPath = request.getContextPath
    val tmpPath = tmpUri.split("/").toList.filter{n => n != null && n.length > 0}

    def processRewrite(uri: String, path: List[String], params: SortedMap[String, String]): (String, List[String], SortedMap[String, String]) = {
      val toMatch = (uri, path, reqType, request)
      if (!rewrite.isDefinedAt(toMatch)) (uri, path, params)
      else {
        val (newUri, newPath, newMap) = rewrite(toMatch)
        processRewrite(newUri, newPath, newMap)
      }
    }
    
    val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    
    val localParams = TreeMap(localSingleParams.map(a => (a._1, List(a._2))).toList :_*)
    
    // val session = request.getSession
    val body = (if (reqType.post_? && request.getContentType == "text/xml") new String(readWholeStream(request.getInputStream), "UTF-8") else "")
      
      val paramNames =  enumToStringList(request.getParameterNames).sort{(s1, s2) => s1 < s2}
    val tmp = paramNames.map{n => (n, request.getParameterValues(n).toList)}
    val params = localParams ++ paramNames.map{n => (n, request.getParameterValues(n).toList)}
    
    new RequestState(paramNames, params,uri,path,contextPath, reqType,/* resourceFinder,*/
		     path.take(1) match {case List("rest") | List("soap") => true; case _ => false},
		     body, request.getContentType)
  }
  
  def nil = new RequestState(Nil, Map.empty, "", Nil, "", GetRequest(false), false, "", "")
}

@scala.serializable
class RequestState(val paramNames: List[String],
		   val params: Map[String, List[String]],
		   val uri: String,val path: List[String],
		   val contextPath: String,
		   val requestType: RequestType,
		   /*val resourceFinder: (String) =>  InputStream,*/
		   val webServices_? : boolean,
		   val body: String,
                   val contentType: String) 
{
  def xml_? = contentType != null && contentType.toLowerCase.startsWith("text/xml")
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s @ _ => s}
  val id = pathParam(0)
  def pathParam(n: int) = head(path.drop(n + 2), "")
  def path(n: int) = head(path.drop(n), null)
  def param(n: String) = params.get(n) match {
    case Some(s :: _) => Some(s)
    case _ => None
  }
  
  def createNotFound = {
    Response(<html><body>The Requested URL {contextPath+this.uri} was not found on this server</body></html>, TreeMap.empty, 404)
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
    Response(<html><body>Exception occured while processing {this.uri} 
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
