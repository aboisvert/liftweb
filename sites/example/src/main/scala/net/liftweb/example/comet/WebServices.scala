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
package net.liftweb.example.comet

import _root_.net.liftweb.http._
import S._
import _root_.javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import _root_.net.liftweb.example.model._

class WebServices (val request: RequestState) {
  def all_users: XmlResponse = {
    XmlResponse(<all_users>{
      User.findAll.map{u => u.toXml}
    }</all_users>)
  }

  def add_user: XmlResponse = {
    var success = false
    for (firstname <- param("firstname");
         lastname <- param("lastname");
         email <- param("email")) {
      val u = new User
      u.firstName(firstname).lastName(lastname).email(email)
      param("textarea").map{v => u.textArea(v)}
      param("password").map{v => u.password(v)}
      success = u.save
    }

    XmlResponse(<add_user success={success.toString}/>)
  }
}
