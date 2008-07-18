package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{Node, Unparsed, Group, NodeSeq}
import net.liftweb.util._
import net.liftweb.util.Helpers._
import javax.servlet.http.Cookie
import js._

trait ToResponse extends ResponseIt {
  def out: Node
  def headers: List[(String, String)]
  def cookies: List[Cookie]
  def code: Int
  def docType: Can[String]

  def toResponse = {
    val encoding =
    (out, headers.ciGet("Content-Type")) match {
    case (up: Unparsed,  _) => ""
    case (_, Empty) | (_, Failure(_, _, _)) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case (_, Full(s)) if (s.toLowerCase.startsWith("text/xml") ||
                          s.toLowerCase.startsWith("text/html") ||
                          s.toLowerCase.startsWith("text/xhtml") ||
			  s.toLowerCase.startsWith("application/xml") ||
			  s.toLowerCase.startsWith("application/xhtml+xml")) => "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    case _ => ""
    }

    val doc = docType.map(_ + "\n") openOr ""

    Response((encoding + doc + AltXML.toXML(out, false, false)).getBytes("UTF-8"), headers, cookies, code)
    }
}

trait ResponseIt {
  def toResponse: Response
}

case class XhtmlResponse(out: Node, docType: Can[String], headers: List[(String, String)],
			 cookies: List[Cookie], code: Int) extends ToResponse


case class JsonResponse(json: JsCmd, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends ResponseIt {
	def toResponse = {
		val bytes = json.toJsCmd.getBytes("UTF-8")
		Response(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "application/json") :: headers, cookies, code)
	}
}

case class Response(data: Array[Byte], headers: List[(String, String)], cookies: List[Cookie], code: Int) extends ResponseIt {
  def toResponse = this

  override def toString="Response("+(new String(data, "UTF-8"))+", "+headers+", "+cookies+", "+code+")"
}

case class RedirectResponse(uri: String, cookies: Cookie*) extends ResponseIt {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = Response(Array(0), List("Location" -> uri), cookies toList, 302)
}

case class RedirectWithState(override val uri: String, state : RedirectState, override val cookies: Cookie*) extends  RedirectResponse(uri, cookies:_*)

object RedirectState {
  //implicit def func2Can(f: () => Unit) = Can(f)

  def apply(f: () => Unit, msgs: (String, NoticeType.Value)*): RedirectState = new RedirectState(Full(f), msgs :_*)
}
case class RedirectState(func : Can[() => Unit], msgs : (String, NoticeType.Value)*)

object MessageState {
  implicit def tuple2MessageState(msg : (String, NoticeType.Value)) = MessageState(msg)
}
case class MessageState(override val msgs : (String, NoticeType.Value)*) extends RedirectState(Empty, msgs:_*)

/**
 * Stock XHTML doctypes available to the lift programmer.
 */
object DocType {
  val xhtmlTransitional = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

  val xhtmlStrict = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

  val xhtmlFrameset = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

  val xhtml11 = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

  val xhtmlMobile = "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.0//EN\" \"http://www.wapforum.org/DTD/xhtml-mobile10.dtd\">"
}

object ResponseInfo {
  var docType: PartialFunction[RequestState, Can[String]] = {
    case _ if S.getDocType._1 => S.getDocType._2
    case _ => Full(DocType.xhtmlTransitional)
  }
}
