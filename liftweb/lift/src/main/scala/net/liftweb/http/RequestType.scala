package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import javax.servlet.http.{HttpServletRequest}
  
abstract class RequestType(val ajax_? : boolean) {
  def post_? : boolean = false
  def get_? : boolean = false
  def head_? : boolean = false
  def put_? : boolean = false
  def delete_? : boolean = false
}

case class GetRequest(aj: boolean) extends RequestType(aj) {
  override def get_? = true
}
case class PostRequest(aj: boolean) extends RequestType(aj) {
  override def post_? = true
}
case class HeadRequest(aj: boolean) extends RequestType(aj) {
  override def head_? = true
}
case class PutRequest(aj: boolean) extends RequestType(aj) {
  override def put_? = true
}
case class DeleteRequest(aj: boolean) extends RequestType(aj) {
  override def delete_? = true
}

object RequestType {
  def apply(req: HttpServletRequest): RequestType = {
    val ajax = req.getHeader("x-requested-with") match {
      case null => false
      case s @ _ => s.toUpperCase == "XMLHttpRequest".toUpperCase
    }
    
    req.getMethod.toUpperCase match {
    case "GET" => GetRequest(ajax)
    case "POST" => PostRequest(ajax)
    case "HEAD" => HeadRequest(ajax)
    case "PUT" => PutRequest(ajax)
    case "DELETE" => DeleteRequest(ajax)
    }
  }
}
