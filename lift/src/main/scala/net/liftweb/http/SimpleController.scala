package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.collection.immutable.TreeMap
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import net.liftweb.util._

/**
 * The base trait of Controllers that handle pre-view requests
 */

 /*
trait SimpleController {
  def request: RequestState
  // private var session: TreeMap[String, Any] = _
  def httpRequest: HttpServletRequest
  
  def param(name: String): Can[String] = {
    request.params.get(name) match {
      case None => Empty
      case Some(nl) => nl.take(1) match {
        case Nil => Empty
        case l => Full(l.head)
      }
    }
  }
  
  def post_? : Boolean = request.post_?
  
  def get(name: String): Can[String] =
    httpRequest.getSession.getAttribute(name) match {
        case null => Empty
        case n: String => Full(n)
        case _ => Empty
  }
  
  def set(name: String, value: String) {
    httpRequest.getSession.setAttribute(name, value)
  }
  
  def unset(name: String) {httpRequest.getSession.removeAttribute(name)}
}
*/
