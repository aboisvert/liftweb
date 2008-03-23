package net.liftweb.http

/*                                                *\
 (c) 2008 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.Node
import net.liftweb.util._
import javax.servlet.http.Cookie

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedResponse(realm: String) extends ResponseIt {
  def toResponse = Response(Array(), List("WWW-Authenticate" -> ("Basic realm=\"" + realm + "\"")), Nil, 401)
}

/**
 * 301 Redirect.
 */
case class PermRedirectResponse(uri: String, request: RequestState, cookies: Cookie*) extends ResponseIt {
  def toResponse = Response(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 301)
}

/**
 * Returning an Atom category document.
 */
case class AtomCategoryResponse(xml: Node) extends ResponseIt {
  def toResponse = XmlMimeResponse(xml, "application/atomcat+xml").toResponse
}

/**
 * Returning an Atom Service Document.
 */
case class AtomServiceResponse(xml: Node) extends ResponseIt {
  def toResponse = XmlMimeResponse(xml, "application/atomsvc+xml").toResponse
}

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

/**
 * Your Request was missing an important element. Use this as a last resort if
 * the request appears incorrect.
 */
case class BadResponse extends ResponseIt {
  def toResponse = Response(Array(), Nil, Nil, 400)
}

/**
 * The Resource was created. We then return the resource, post-processing, to 
 * the client.
 */
case class CreatedResponse(xml: Node, mime: String) extends ToResponse {
  def docType = Empty
  def code = 201
  def headers = List("Content-Type" -> mime)
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
case class AtomCreatedResponse(xml: Node) extends ResponseIt {
  def toResponse = CreatedResponse(xml, "application/atom+xml").toResponse
}

/**
 * Basic 200 response but without body.
 */
case class OkResponse extends ResponseIt {
  def toResponse = Response(Array(), Nil, Nil, 200)
}

/**
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances. 
 */
case class MethodNotAllowedResponse extends ResponseIt {
  def toResponse = Response(Array(), Nil, Nil, 405)
}

/**
 * The requested Resource does not exist.
 */
case class NotFoundResponse extends ResponseIt {
  def toResponse = Response(Array(), Nil, Nil, 404)
}

/**
 * The requested Resource used to exist but no longer does. 
 */
case class GoneResponse extends ResponseIt {
  def toResponse = Response(Array(), Nil, Nil, 410)
}
