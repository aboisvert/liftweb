package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import _root_.scala.xml.Node
import _root_.net.liftweb.util._
import js._
import _root_.javax.servlet.http.Cookie

/**
 * Allows you to create custom 200 responses for clients using different
 * Content-Types.
 */
case class XmlMimeResponse(xml: Node, mime: String) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> mime)
  def cookies = Nil
  def out = xml
}

case class XmlResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "text/xml")
  def cookies = Nil
  def out = xml
}

/**
 * Returning an Atom document.
 */
case class AtomResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "application/atom+xml")
  def cookies = Nil
  def out = xml
}

/**
 * Returning an OpenSearch Description Document.
 */
case class OpenSearchResponse(xml: Node) extends ToResponse {
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

