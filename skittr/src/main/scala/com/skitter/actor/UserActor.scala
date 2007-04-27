package com.skitter.actor

import scala.actors._
import scala.actors.Actor._
import com.skitter.model._
// import com.skitter.model.MsgStore._
import scala.collection.mutable.{HashMap}
import net.liftweb.mapper._
import java.util.concurrent.locks.ReentrantReadWriteLock
import net.liftweb.util._
import net.liftweb.util.Helpers._

class UserActor extends Actor {
  private var user: User = _
  private var latestMsgs: List[Message] = Nil
  private var followers: List[Actor] = Nil
  private var timelineViewers: List[Actor] = Nil
  private var messageViewers: List[Actor] = Nil
  private var localTimeline: List[Message] = Nil
  
  private def nextTime: long = 2.minutes + Helpers.randomLong(5.minutes)
  
  def act = {
    this.trapExit = true
    ActorPing.schedule(this, SendMessage("Hello @ "+(new java.util.Date), "auto"), nextTime)
    loop
  }
  
  def loop {
    react {
    case SendMessage(text, src) =>
    ActorPing.schedule(this, SendMessage("Hello @ "+(new java.util.Date), "auto"), nextTime)
    val msg = Message(text, System.currentTimeMillis, user.name.get, src)
    latestMsgs = (msg :: latestMsgs).take(50)
    messageViewers.foreach(_ ! Messages(latestMsgs))
    followers.foreach(_ ! msg)
    this ! msg
    MsgStore.create.message(text).source(src).who(user.id).save
    loop
    
    case GetMessages =>
    reply(Messages(latestMsgs))
    loop
    
    case GetTimeline =>
    reply(Timeline(localTimeline))
    loop
    
    case GetUser =>
    reply(user)
    loop
    
    case Bye =>
    UserList.remove(user.name)
    this.exit(Bye)
    loop
    
    case Setup(theUser) =>
    user = theUser
    UserList.add(user.name, this)
    latestMsgs = MsgStore.findAll(By(MsgStore.who, user.id.get), 
                                  OrderBy(MsgStore.when, false),
                                  MaxRows(50)).map(s => Message(s.message, s.when, user.name, s.source))
    localTimeline = latestMsgs
    loop
    
    case ConfigFollowers =>
    followers = User.findAll(BySql("id IN (SELECT friend FROM friends WHERE owner = ?)", user.id.get)).
      flatMap(u => UserList.find(u.name).toList)
    followers.foreach(_ ! MergeIntoTimeline(latestMsgs))
    loop
    
    case MergeIntoTimeline(msg) =>
    localTimeline = (localTimeline ::: msg).sort((a,b) => b.when < a.when).take(50)
    loop
    
    case AddTimelineViewer =>
    timelineViewers = sender :: timelineViewers
    this.link(sender)
    loop
    
    case RemoveTimelineViewer =>
    timelineViewers = timelineViewers.remove(_ == sender)
    this.unlink(sender)
    loop

    case AddMessageViewer =>
    messageViewers = sender :: messageViewers
    this.link(sender)
    loop
    
    case RemoveMessageViewer =>
    messageViewers = messageViewers.remove(_ == sender)
    this.unlink(sender)
    loop
        
    case AddFollower(who) =>
    followers = who :: followers
    who ! MergeIntoTimeline(latestMsgs)
    loop

    case RemoveFollower(who) =>
    followers = followers.remove(_ == who)
    loop

    case msg : Message =>
    localTimeline = (msg :: localTimeline).take(50)
    timelineViewers.foreach(_ ! Timeline(localTimeline))
    loop

    case ('EXIT, who : Actor, _) =>
    messageViewers = messageViewers.remove(_ == who)
    timelineViewers = timelineViewers.remove(_ == who)
    loop
  }
  }
}

object UserList {
  private val set = new HashMap[String, UserActor]()
  private val rwl = new ReentrantReadWriteLock
  private val r = rwl.readLock
  private val w = rwl.writeLock
  
  def randomUsers = {
    r.lock
    try {
    for (val u <- set;
         shouldShow(1)) yield u._1
    } finally {
      r.unlock
    }
  }
  
  def create {
    User.findAll.map{u =>
    val ua = new UserActor
    ua.start
    ua ! Setup(u)
    ua
    }.foreach (_ ! ConfigFollowers)
  }
  
  def startUser(who: User) {
    if (who.shouldStart_?) {
    val ua = new UserActor
    ua.start
    ua ! Setup(who)
    ua ! ConfigFollowers
    }
  }
  
  def shutdown = foreach(_ ! Bye)
  
  def foreach(f: UserActor => Any) {
    r.lock
    try {
    set.foreach(i => f(i._2))
    } finally {
      r.unlock
    }
  }
  
  def add(name: String, who: UserActor) {
    w.lock
    try {
    set(name) = who
    } finally {
      w.unlock
    }
  }
  
  def find(name: String): Option[UserActor] = {
    r.lock
    try {
    set.get(name)
    } finally {
      r.unlock
    }
  }
  
  def remove(name: String) {
    w.lock
    try {
    set -= name
    } finally {
      w.unlock
    }
  }
}

sealed abstract class UserMsg
case class SendMessage(text: String, src: String) extends UserMsg
case class Message(text: String, when: long, who: String, src: String) extends UserMsg
case class Setup(user: User) extends UserMsg
case object AddTimelineViewer extends UserMsg
case object RemoveTimelineViewer extends UserMsg
case object AddMessageViewer extends UserMsg
case object RemoveMessageViewer extends UserMsg
case class AddFollower(who: UserActor) extends UserMsg
case class RemoveFollower(who: UserActor) extends UserMsg
case class MergeIntoTimeline(what: List[Message]) extends UserMsg
case object GetMessages extends UserMsg
case object GetTimeline extends UserMsg
case object Bye extends UserMsg
case object ConfigFollowers extends UserMsg
case object GetUser extends UserMsg
case class Timeline(messages: List[Message]) extends UserMsg
case class Messages(messages: List[Message]) extends UserMsg
