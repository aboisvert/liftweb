package com.skittr.controller

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util.{Helpers, Can, Full, Empty, Failure}
import scala.xml._
import com.skittr.actor._
import S._
import com.skittr.model.{Friend, User}
import net.liftweb.mapper._

class WatchUser extends ControllerActor {
  private var userActor: Can[UserActor] = Empty
  private var messages: List[Message] = Nil
  
  def render: NodeSeq = {
    val inputName = uniqueId+"_msg"
    (for (ua <- userActor;
          user <- (ua !? (400L, GetUserIdAndName)) match {case Some(u: UserIdInfo) => Full(u)
							      case _ => Empty}) yield {
	    S.addFunctionMap(inputName,{in: List[String] => in.foreach(m =>  ua ! SendMessage(m, "web")); true})

	    Helpers.bind("sk", defaultXml, "username" -> (user.name+" -> "+user.fullName), 
			 "content" -> <span>{friendList(user) ++
				       <lift:form method="post" action=".">
				       <textarea name={inputName} cols='40'></textarea><br />
				       <input type="submit" value="msg"/>
				       </lift:form>}</span>) ++ 
	    messages.flatMap(msg => 
	      Helpers.bind("sk", defaultXml,
			   "username" -> (msg.who+" @ "+toInternetDate(msg.when)),
			   "content" -> msg.text))
	  }) openOr Helpers.bind("sk", defaultXml, "username" -> "N/A", "content" -> "N/A")
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
  
  private def friendList(user: UserIdInfo): NodeSeq = <ul>{
    user.friends.map(f => <li><a href={"/user/"+f}>{f}</a>&nbsp;<a href={"/unfriend/"+f}>Unfriend</a></li>)
  }</ul>
}
