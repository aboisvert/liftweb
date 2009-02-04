/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.http

import _root_.scala.xml.{Node, Unparsed, Group, NodeSeq}
import _root_.net.liftweb.util._
import _root_.javax.servlet.http.Cookie
import js._
import _root_.net.liftweb.util.Helpers._

/**
 * 200 response but without body.
 */
case class OkResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 200)
}

trait HeaderStuff {
   val headers = S.getHeaders(Nil)
   val cookies = S.responseCookies
}

/**
 * 201 Created Response
 *
 * The Resource was created. We then return the resource, post-processing, to
 * the client. Usually used with HTTP PUT.
 */
case class CreatedResponse(xml: Node, mime: String) extends NodeResponse {
  def docType = Empty
  def code = 201
  def headers = List("Content-Type" -> mime)
  def cookies = Nil
  def out = xml
}

/**
 * 202 response but without body.
 */
case class AcceptedResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 202)
}

/**
 * 204 response but without body.
 */
case class NoContentResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 204)
}

/**
 * 205 response but without body.
 */
case class ResetContentResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 205)
}

/**
 * 301 Redirect.
 */
case class PermRedirectResponse(uri: String, request: Req, cookies: Cookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 301)
}

/**
 * 307 Redirect.
 */
case class TemporaryRedirectResponse(uri: String, request: Req, cookies: Cookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 307)
}

/**
 * 400 Bad Request
 *
 * Your Request was missing an important element. Use this as a last resort if
 * the request appears incorrect.
 */
case class BadResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 400)
}

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedResponse(realm: String) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("WWW-Authenticate" -> ("Basic realm=\"" + realm + "\"")), Nil, 401)
}

object Qop extends Enumeration(0, "auth", "auth-int", "auth,auth-int") {
  val AUTH, AUTH_INT, AUTH_AND_AUTH_INT = Value
}

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedDigestResponse(override val realm: String, qop: Qop.Value, nonce: String, opaque: String) extends UnauthorizedResponse(realm) {
  override def toResponse = InMemoryResponse(Array(), List("WWW-Authenticate" -> (
        "Digest realm=\"" + realm + "\", " +
        "qop=\"" + qop + "\", " +
        "nonce=\"" + nonce + "\", " +
        "opaque=\"" + opaque + "\""
      )), Nil, 401)
}

/**
 * 403 Forbidden
 *
 * The server understood the request, but is refusing to fulfill it. 
 * Authorization will not help and the request SHOULD NOT be repeated.
 */
case class ForbiddenResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 403)
}

/**
 * 404 Not Found
 *
 * The server has not found anything matching the Request-URI.
 */
case class NotFoundResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 404)
}

/**
 * 405 Method Not Allowed
 *
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances.
 */
case class MethodNotAllowedResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 405)
}

/**
 * 406 Not Acceptable
 *
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances.
 */
case class NotAcceptableResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 406)
}

/**
 * 410 Resource Gone
 *
 * The requested Resource used to exist but no longer does.
 */
case class GoneResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 410)
}

/**
 * 415 Resource Gone
 *
 * The requested Resource used to exist but no longer does.
 */
case class UnsupportedMediaTypeResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 415)
}

/**
 * 500 Internal Server Error
 *
 * The server encountered an unexpected condition which prevented 
 * it from fulfilling the request.
 */
case class InternalServerErrorResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 500)
}

/**
 * 501 Not Implemented
 *
 * The server does not support the functionality required to 
 * fulfill the request. This is the appropriate response when the 
 * server does not recognize the request method and is not capable 
 * of supporting it for any resource.
 */
case class NotImplementedResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 501)
}

/**
 * 502 Bad Gateway
 *
 * The server, while acting as a gateway or proxy, received an invalid 
 * response from the upstream server it accessed in attempting 
 * to fulfill the request.
 */
case class BadGatewayResponse() extends LiftResponse with HeaderStuff {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 502)
}

/**
 * 503 Bad Gateway
 *
 * The server, while acting as a gateway or proxy, received an invalid 
 * response from the upstream server it accessed in attempting 
 * to fulfill the request.
 */
case class ServiceUnavailableResponse(retryAfter: Long) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Retry-After" -> retryAfter.toString), Nil, 503)
}

object JavaScriptResponse {
  def apply(js: JsCmd): LiftResponse = JavaScriptResponse(js, S.getHeaders(Nil), S.responseCookies, 200)
}

/**
 * Impersonates a HTTP response having Content-Type = text/javascript
 */
case class JavaScriptResponse(js: JsCmd, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/javascript") :: headers, cookies, code)
  }
}

object JSONResponse {
  def apply(js: JsExp): LiftResponse = JSONResponse(js, S.getHeaders(Nil), S.responseCookies, 200)
}

/**
 * Impersonates a HTTP response having Content-Type = text/json
 */
case class JSONResponse(js: JsExp, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/json") :: headers, cookies, code)
  }
}

object PlainTextResponse {
  def apply(text: String): PlainTextResponse = PlainTextResponse(text, Nil, 200)
  def apply(text: String, code: Int): PlainTextResponse = PlainTextResponse(text, Nil, code)
}

case class PlainTextResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = text.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/plain") :: headers, Nil, code)
  }
}

object CSSResponse {
  def apply(text: String): CSSResponse = CSSResponse(text, Nil, 200)
  def apply(text: String, code: Int): CSSResponse = CSSResponse(text, Nil, code)
}

case class CSSResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = text.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/css") :: headers, Nil, code)
  }
}

trait NodeResponse extends LiftResponse {
  def out: Node
  def headers: List[(String, String)]
  def cookies: List[Cookie]
  def code: Int
  def docType: Box[String]
  def renderInIEMode: Boolean = false

  def toResponse = {
    val encoding: String =
    (out, headers.ciGet("Content-Type")) match {
      case (up: Unparsed,  _) => ""

      case (_, Empty) | (_, Failure(_, _, _)) =>
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

      case (_, Full(s)) if (s.toLowerCase.startsWith("text/html")) =>
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

      case (_, Full(s)) if (s.toLowerCase.startsWith("text/xml") ||
                            s.toLowerCase.startsWith("text/xhtml") ||
                            s.toLowerCase.startsWith("application/xml") ||
                            s.toLowerCase.startsWith("application/xhtml+xml")) =>
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

      case _ => ""
    }

    val doc = docType.map(_ + "\n") openOr ""

    val sb = new StringBuilder(64000)

    sb.append(encoding)
    sb.append(doc)
    AltXML.toXML(out, _root_.scala.xml.TopScope,
                 sb, false, false, renderInIEMode)

    sb.append("  \n  ")

    val ret = sb.toString

    InMemoryResponse(ret.getBytes("UTF-8"), headers, cookies, code)
  }
}

case class XhtmlResponse(out: Node, docType: Box[String],
                         headers: List[(String, String)],
                         cookies: List[Cookie],
                         code: Int,
                         override val renderInIEMode: Boolean) extends NodeResponse


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

