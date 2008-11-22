package net.liftweb.http

/*
 (c) 2008 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
*/

import _root_.scala.xml.Node
import _root_.net.liftweb.util._
import _root_.javax.servlet.http.Cookie

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