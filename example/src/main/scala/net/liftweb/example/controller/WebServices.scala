package net.liftweb.example.controller

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.http._
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import net.liftweb.example.model._

class WebServices (rstate: RequestState, httpRequest_l: HttpServletRequest) extends SimpleController {
  this.request = rstate
  this.httpRequest = httpRequest_l
  
  def all_users: XmlResponse = {
    XmlResponse(<all_users>{
      User.findAll.map{u => u.toXml}
    }</all_users>)
  }
  
  def add_user: XmlResponse = {
    var success = false
    for (val firstname <- params("firstname");
         val lastname <- params("lastname");
         val email <- params("email")) {
      val u = new User
      u.firstName := firstname
      u.lastName := lastname
      u.email := email
      params("textarea").map{v => u.textArea := v}
      params("password").map{v => u.password := v}
      success = u.save
    }
    
    XmlResponse(<add_user success={success.toString}/>)
  }
}
