package com.skitter.controller

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import scala.xml._
import com.skitter.actor._

class WatchUser extends ControllerActor {
  private var userActor: Option[UserActor] = None
  private var messages: List[Message] = Nil
  
  def render: NodeSeq = {
    Helpers.bind("sk", defaultXml, "username" -> name, "content" -> "") ++ 
    messages.flatMap{
      msg => 
      Helpers.bind("sk", defaultXml, "username" -> (msg.who+" @ "+toInternetDate(msg.when)), "content" -> msg.text)
    }
  }
  
  override def lowPriority : PartialFunction[Any, Unit] = {
    val ret: PartialFunction[Any, Unit] = {case Messages(msg) =>
      messages = msg
      reRender
      loop
    }
    
    ret orElse super.lowPriority
  } 

  
  override def localSetup {
    userActor = UserList.find(name) 
    userActor.foreach{ua => ua ! AddTimelineViewer ;  messages = (ua !? GetTimeline) match {case Messages(m) => m; case _ => Nil}}
  }
}
