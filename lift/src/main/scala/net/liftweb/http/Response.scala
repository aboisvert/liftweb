package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import _root_.scala.xml.{Node, Unparsed, Group, NodeSeq}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.javax.servlet.http.Cookie
import js._
import _root_.java.io.InputStream

trait ToResponse extends LiftResponse {
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

    val sb = new StringBuilder(64000)
    sb.append(encoding)
    sb.append(doc)
    AltXML.toXML(out, _root_.scala.xml.TopScope, sb, false, false)
    sb.append("  \n  ")

    val ret = sb.toString // (encoding + doc + AltXML.toXML(out, false, false))

    InMemoryResponse(ret.getBytes("UTF-8"), headers, cookies, code)
    }
}

trait LiftResponse {
  def toResponse: BasicResponse
}

case class XhtmlResponse(out: Node, docType: Can[String], headers: List[(String, String)],
			 cookies: List[Cookie], code: Int) extends ToResponse

object JsonResponse {
  def apply(json: JsExp): LiftResponse = JsonResponse(json, Nil, Nil, 200)
}

case class JsonResponse(json: JsExp, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends LiftResponse {
	def toResponse = {
		val bytes = json.toJsCmd.getBytes("UTF-8")
		InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "application/json") :: headers, cookies, code)
	}
}

sealed trait BasicResponse extends LiftResponse {
  def headers: List[(String, String)]
  def cookies: List[Cookie]
  def code: Int
  def size: Long
}

final case class InMemoryResponse(data: Array[Byte], headers: List[(String, String)], cookies: List[Cookie], code: Int) extends BasicResponse {
  def toResponse = this
  def size = data.length

  override def toString="InMemoryResponse("+(new String(data, "UTF-8"))+", "+headers+", "+cookies+", "+code+")"
}

final case class StreamingResponse(data: {def read(buf: Array[Byte]): Int}, onEnd: () => Unit, size: Long, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends BasicResponse {
  def toResponse = this

    override def toString="StreamingResponse( steaming_data , "+headers+", "+cookies+", "+code+")"
}

case class RedirectResponse(uri: String, cookies: Cookie*) extends LiftResponse {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = InMemoryResponse(Array(0), List("Location" -> uri), cookies toList, 302)
}

object DoRedirectResponse {
  def apply(url: String): LiftResponse = RedirectResponse(url, Nil :_*)
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
  var docType: PartialFunction[Req, Can[String]] = {
    case _ if S.getDocType._1 => S.getDocType._2
    case _ => Full(DocType.xhtmlTransitional)
  }
}
