package com.liftweb.wiki.snippet

import scala.xml._
import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import com.liftweb.wiki.model._

/** Snippet handling user login and registration.
 */
class Registrar {

  /** Render the login panel.
   */
  def render(xhtml: Group) =
    S.get("username") match {
      case Some(name) =>
        <span><b>{ name }</b>,
          <a href="/logout">log out</a></span>
      
      case None =>
        var username, password = ""
        def doLogin(ignore: String) {
          User.log(username, password) match {
            case Some(user) =>
              S.set("username", user.username.get)
            case None =>
              S.error("invalid username or password")
          }
          S.redirectTo("/")
        }

        <form method="POST" action={ S.request.uri }>
          Username: { S.text("", username = _) }
          Password: { S.text("", password = _) }
          { S.submit("login", doLogin _) }
          or <a href="/register">register</a>
        </form>
    }

  /** Log out the currently logged in user.
   */
  def logout = {
    if (logged) {
      S.unset("username")
      S.notice("log out successfuly")
    } else {
      S.error("can't log out if you are not logged in")
    }
    S.redirectTo("/")
  }

  /** Register a new user.
   */
  def register = {
    if (logged) {
      S.error("can't register if you're logged in")
      S.redirectTo("/")
    } else {
      val user = new User
      def doRegister(in: List[String]): Boolean = {
        val issues = user.validate
        if (issues.isEmpty && User.findAll(By(User.username, user.username.get)).isEmpty) {
          user.save
          S.set("username", user.username)
          S.notice("user created")
          S.redirectTo("/")
        } else {
          S.error("issues while creating user, a user with this username may already exist")
        }
        true
      }
      <form method="POST" action={ S.request.uri }>
        { user.toForm(doRegister) }
        <input type="submit" value="Register" />
      </form>
    }
  }

  private def logged = S.get("username").isDefined
}
