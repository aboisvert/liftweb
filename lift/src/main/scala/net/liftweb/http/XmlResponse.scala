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

case class XmlResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "text/xml")
  def cookies = Nil
  def out = xml
}

object JavaScriptResponse {
  def apply(js: JsCmd): LiftResponse = JavaScriptResponse(js, Nil, Nil, 200)
}

case class JavaScriptResponse(js: JsCmd, headers: List[(String, String)], cookies: List[Cookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/javascript") :: headers, cookies, code)
  }
}

