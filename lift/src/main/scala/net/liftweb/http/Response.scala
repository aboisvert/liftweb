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

import _root_.net.liftweb.util._
import _root_.javax.servlet.http.Cookie
import js._
import _root_.java.io.InputStream

trait LiftResponse {
  def toResponse: BasicResponse
}

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
  def apply(f: () => Unit, msgs: (String, NoticeType.Value)*): RedirectState = new RedirectState(Full(f), msgs :_*)
}
case class RedirectState(func : Box[() => Unit], msgs : (String, NoticeType.Value)*)

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
  var docType: PartialFunction[Req, Box[String]] = {
    case _ if S.getDocType._1 => S.getDocType._2
    case _ => Full(DocType.xhtmlTransitional)
  }
}
