package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServletRequest}

abstract class RequestType {
  def post_? : boolean = false
  def get_? : boolean = false
  def head_? : boolean = false
  def put_? : boolean = false
  def delete_? : boolean = false
}

case object GetRequest extends RequestType {
  override def get_? = true
}
case object PostRequest extends RequestType {
  override def post_? = true
}
case object HeadRequest extends RequestType {
  override def head_? = true
}
case object PutRequest extends RequestType {
  override def put_? = true
}
case object DeleteRequest extends RequestType {
  override def delete_? = true
}

case class UnkownRequest(method: String) extends RequestType

object RequestType {
  def apply(req: HttpServletRequest): RequestType = {
    /*
    val ajax = req.getHeader("x-requested-with") match {
      case null => false
      case s @ _ => s.toUpperCase == "XMLHttpRequest".toUpperCase
    }*/
    
    req.getMethod.toUpperCase match {
      case "GET" => GetRequest
      case "POST" => PostRequest
      case "HEAD" => HeadRequest
      case "PUT" => PutRequest
      case "DELETE" => DeleteRequest
      case meth => UnkownRequest(meth)
    }
  }
}
