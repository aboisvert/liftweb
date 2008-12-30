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