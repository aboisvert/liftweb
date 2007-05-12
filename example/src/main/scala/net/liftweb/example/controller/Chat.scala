package net.liftweb.example.controller

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import scala.collection.immutable.TreeMap
import scala.xml._
import S._

class Chat extends ControllerActor {
  private var userName = ""
  private var currentData: List[ChatLine] = Nil
  
  private val server = {
    val ret = ChatServer.server
    ret !? ChatServerAdd(this) match {
      case ChatServerUpdate(value) => currentData = value
      case _ => {}
    }
    ret
  }
  

  override def lowPriority : PartialFunction[Any, Unit] = {
    val ret: PartialFunction[Any, Unit] = {
      case ChatServerUpdate(value) => 
      currentData = value
      reRender
    } 
    
    ret orElse super.lowPriority
  } 
  
  def render = {
try {
    val inputName = this.uniqueId+"_msg"
    
    S.addFunctionMap(inputName, &sendMessage)
    
    <span>Hello "{userName}"<ul>{
      currentData.reverse.map{
        cl =>
          <li>{hourFormat(cl.when)} {cl.user}: {cl.msg}</li>
      }.toList
    }</ul><lift:form method="POST" action=".">
    <input name={inputName} type="text" value=""/><input value="Send" type="submit"/>
    </lift:form></span>
} catch {
  case e => e.printStackTrace ; Text("")
}
  }
  
  override def localSetup {
    if (userName.length == 0) {
      ask(new AskName, "what's your username") {
	answer =>
	  answer match {
            case s : String if (s.length > 2) => userName = s; true
            case _ => localSetup; true
	  }
      }
    }
  }
  
  def waitForUpdate : Option[List[ChatLine]] = {
    receiveWithin(100) {
      case ChatServerUpdate(l) => Some(l)
      case TIMEOUT => None
    }
  }
  
  def sendMessage(in: List[String]) = {
    server ! ChatServerMsg(userName, in.head)
    waitForUpdate match {
      case Some(l : List[ChatLine]) => currentData = l ; true
      case _ => true
    }
  }
}
