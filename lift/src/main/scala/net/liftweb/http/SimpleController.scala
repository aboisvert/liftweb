package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.collection.immutable.TreeMap
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}

/**
 * The base trait of Controllers that handle pre-view requests
 */
trait SimpleController {
  def request: RequestState
  // private var session: TreeMap[String, Any] = _
  def httpRequest: HttpServletRequest
  
  def params(name: String): Option[String] = {
    request.params.get(name) match {
      case None => None
      case Some(nl) => nl.take(1) match {
        case Nil => None
        case l => Some(l.head)
      }
    }
  }
  
  def post_? : boolean = request.post_?
  
  def get(name: String): Option[String] = {
    httpRequest.getSession.getAttribute(name) match {
        case null => None
        case n => {
      if (n.isInstanceOf[String]) Some(n.asInstanceOf[String])
      else None
        }
  }
  }
  
  def set(name: String, value: String) {
    httpRequest.getSession.setAttribute(name, value)
  }
  
  def unset(name: String) {httpRequest.getSession.removeAttribute(name)}
      
  /*
   def apply(name: String): Option[Any] = {
   session.get(name)
   }*/
  
 /* def apply[T](name: String): Option[T] = {
    if (httpRequest == null) None
    else {
      httpRequest.getSession.getAttribute(name) match {
	case null => None
	case n => {
          if (n.isInstanceOf[T]) Some(n.asInstanceOf[T])
          else None
	}
      }
    }
  }
  
  def update(name: String, value: Any) {
    value match {
      case null => httpRequest.getSession.removeAttribute(name)
      case _ => {httpRequest.getSession.setAttribute(name, value)}
    }
  }
  */
}
