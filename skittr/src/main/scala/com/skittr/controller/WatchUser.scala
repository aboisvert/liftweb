package com.skittr.controller

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import scala.xml._
import com.skittr.actor._

class WatchUser extends ControllerActor {
  private var userActor: Option[UserActor] = None
  private var messages: List[Message] = Nil
  
  def render: NodeSeq = {
    val inputName = uniqueId+"_msg"

      S.addFunctionMap(inputName,{in => in.foreach(m =>  userActor.foreach(_ ! SendMessage(m, "web"))); true})
      
    Helpers.bind("sk", defaultXml, "username" -> name, "content" -> <lift:form method="post" action=".">
    <input name={inputName} type="text"/>
    <input type="submit" value="msg"/>
    </lift:form>) ++ 
    messages.flatMap{
      msg => 
      Helpers.bind("sk", defaultXml, "username" -> (msg.who+" @ "+toInternetDate(msg.when)), "content" -> msg.text)
    }
  }
  
  override def lowPriority : PartialFunction[Any, Unit] = {
    val ret: PartialFunction[Any, Unit] = {
      case Timeline(msg) =>
      messages = msg
      reRender
    }
    
    ret orElse super.lowPriority
  } 

  
  override def localSetup {
    userActor = UserList.find(name) 
    userActor.foreach{ua => ua ! AddTimelineViewer ;  messages = (ua !? GetTimeline) match {case Timeline(m) => m; case _ => Nil}}
  }
}
