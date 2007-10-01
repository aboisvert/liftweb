package net.liftweb.example.comet

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.collection.immutable.TreeMap
import scala.xml._
import S._
import net.liftweb.util._

class Chat(theSession: Session, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]) extends 
      CometActor(theSession, name, defaultXml, attributes) {
  private var userName = ""
  private var currentData: List[ChatLine] = Nil
  def defaultPrefix = "chat"

  private val server = {
    val ret = ChatServer.server
    (ret !? ChatServerAdd(this)) match {
      case ChatServerUpdate(value) => currentData = value
      case _ => 
    }
    ret
  }

  override def lowPriority : PartialFunction[Any, Unit] = {
      case ChatServerUpdate(value) => 
      currentData = value
      reRender
    } 
  
  def render = <span>Hello "{userName}" <a href="/chat">Try</a>
    <ul>{currentData.reverse.flatMap(cl => <li>{hourFormat(cl.when)} {cl.user}: {cl.msg}</li>)}</ul>
    {ajaxForm(text("",msg => sendMessage(msg)) ++ submit("Send", ignore => true))}</span>
  
  override def localSetup {
    if (userName.length == 0) {
      ask(new AskName(theSession, name, defaultXml, attributes), "what's your username") {
            case s: String if (s.trim.length > 2) => userName = s.trim; reRender
            case s => localSetup; reRender
      }
    }
  }
  
  def waitForUpdate : Option[List[ChatLine]] = receiveWithin(100) {case ChatServerUpdate(l) => Some(l) case TIMEOUT => None}
  
  def sendMessage(msg: String) {
    server ! ChatServerMsg(userName, msg)
    waitForUpdate match {
      case Some(l : List[ChatLine]) => currentData = l ; reRender
      case _ => 
    }
  }
}
