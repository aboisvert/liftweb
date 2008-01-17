package net.liftweb.example.comet

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.collection.immutable.TreeMap
import scala.xml._
import S._
import net.liftweb.util._
import net.liftweb.http.js._
import JsCmds._


class Chat(theSession: LiftSession, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]) extends 
      CometActor(theSession, name, defaultXml, attributes) {
  private var userName = ""
  private var currentData: List[ChatLine] = Nil
  def defaultPrefix = "chat"
    
  private lazy val infoId = uniqueId + "_info"

  private val server = {
    val ret = ChatServer.server
    ret ! ChatServerAdd(this)
    ret
  }

  override def lowPriority = {
    case ChatServerUpdate(value) => 
      val diff = value diff currentData
    if (!diff.isEmpty) partialUpdate(diff.reverse.foldLeft(Noop)((a,b) => a ++ AppendHtml(infoId, line(b))))
      currentData = value // ; reRender(false)
  } 
  
  override lazy val fixedRender: Can[NodeSeq] = {
    val n = "id"+randomString(10)
    ajaxForm(After(100, SetValueAndFocus(n, "")), 
        (S.text("", sendMessage _) % ("id" -> n)) ++ <input type="submit" value="Chat"/> )
  }
  
  def line(cl: ChatLine) = (<li>{hourFormat(cl.when)} {cl.user}: {cl.msg}</li>)
  
  override def render = (<span>Hello "{userName}"
    <ul id={infoId}>{currentData.reverse.flatMap(line)}</ul>
    </span>)
  
  override def localSetup {
    if (userName.length == 0) {
      ask(new AskName(theSession, name, defaultXml, attributes), "what's your username") {
            case s: String if (s.trim.length > 2) => userName = s.trim; reRender(true)
            case s => localSetup; reRender(false)
      }
    }
  }
  
	// def waitForUpdate : Option[List[ChatLine]] = receiveWithin(100) {case ChatServerUpdate(l) => Some(l) case TIMEOUT => None}
  
  def sendMessage(msg: String) = server ! ChatServerMsg(userName, msg.trim)

/*
msg.trim match {
    case msg if msg.length > 0 => 
    server ! ChatServerMsg(userName, msg)
    waitForUpdate match {
      case Some(l : List[ChatLine]) => currentData = l ; reRender(false)
      case _ => 
    }
    case _ =>
  }
*/
}
