package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import _root_.scala.xml.{Node, Unparsed, Group, NodeSeq}
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import js._
import _root_.javax.servlet.http.Cookie

trait NodeResponse extends LiftResponse {
  def out: Node
  def headers: List[(String, String)]
  def cookies: List[Cookie]
  def code: Int
  def docType: Can[String]

  def toResponse = {
    val (encoding: String, ieMode: Boolean) =
    (out, headers.ciGet("Content-Type")) match {
    case (up: Unparsed,  _) => ("", true)
    
    case (_, Empty) | (_, Failure(_, _, _)) => 
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", true)

    case (_, Full(s)) if (s.toLowerCase.startsWith("text/html")) =>
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", true)

    case (_, Full(s)) if (s.toLowerCase.startsWith("text/xml") ||
                          s.toLowerCase.startsWith("text/xhtml") ||
			  s.toLowerCase.startsWith("application/xml") ||
			  s.toLowerCase.startsWith("application/xhtml+xml")) =>
      ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", false)

    case _ => ("", true)
    }

    val doc = docType.map(_ + "\n") openOr ""

    val sb = new StringBuilder(64000)

    sb.append(encoding)
    sb.append(doc)
    AltXML.toXML(out, _root_.scala.xml.TopScope,
		 sb, false, false, ieMode)

    sb.append("  \n  ")

    val ret = sb.toString

    InMemoryResponse(ret.getBytes("UTF-8"), headers, cookies, code)
    }
}

case class XhtmlResponse(out: Node, docType: Can[String], headers: List[(String, String)],
			 cookies: List[Cookie], code: Int) extends NodeResponse


/**
 * Allows you to create custom 200 responses for clients using different
 * Content-Types.
 */
case class XmlMimeResponse(xml: Node, mime: String) extends NodeResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> mime)
  def cookies = Nil
  def out = xml
}

case class XmlResponse(xml: Node) extends NodeResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "text/xml")
  def cookies = Nil
  def out = xml
}

/**
 * Returning an Atom document.
 */
case class AtomResponse(xml: Node) extends NodeResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "application/atom+xml")
  def cookies = Nil
  def out = xml
}

/**
 * Returning an OpenSearch Description Document.
 */
case class OpenSearchResponse(xml: Node) extends NodeResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "application/opensearchdescription+xml")
  def cookies = Nil
  def out = xml
}

/**
 * The Atom entity was successfully created and is shown to the client.
 */
case class AtomCreatedResponse(xml: Node) extends LiftResponse {
  def toResponse = CreatedResponse(xml, "application/atom+xml").toResponse
}

/**
 * Returning an Atom category document.
 */
case class AtomCategoryResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomcat+xml").toResponse
}

/**
 * Returning an Atom Service Document.
 */
case class AtomServiceResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomsvc+xml").toResponse
}

