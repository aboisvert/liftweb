package net.liftweb.ext_api.facebook;

import net.liftweb.http.{S, SessionVar}
import net.liftweb.util.Helpers._
import net.liftweb.util.{Can, Empty, Failure, Full}

object Facebook {
  object API extends SessionVar[FacebookClient](sessionKey.map(k => new FacebookClient(k)))
  
  def canvasPage_? : boolean = S.param("fb_sig_in_canvas") match {
    case Full(num) if toInt(num) == 1 => true
    case _ => false
  }
  
  def addedApplication_? : boolean = S.param("fb_sig_added") match {
    case Full(num) if toInt(num) == 1 => true
    case _ => false
  }
  
  def loggedIn_? : boolean = S.param("fb_sig_user") match {
    case Full(num) if toInt(num) > 0 => true
    case _ => false
  }
  
  def userId: Can[Int] = S.param("fb_sig_user") match {
    case Full(num) => Full(toInt(num))
    case _ => Empty
  }
  
  def userId_! : Int = userId.open_!
  
  def sessionKey : Can[String] = S.param("fb_sig_session_key")
  
  def sessionKey_! : String = sessionKey.open_!
  
  def loginUrl: String = "http://www.facebook.com/login.php?api_key=" + FacebookRestApi.apiKey + "&v=1.0"
}