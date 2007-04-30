package com.skittr.actor

import scala.actors._
import scala.actors.Actor._
import com.skittr.model._
import scala.collection.mutable.{HashMap}
import net.liftweb.mapper._
import java.util.concurrent.locks.ReentrantReadWriteLock
import net.liftweb.util._
import net.liftweb.util.Helpers._

/**
  * All the "current state" and logic necessary to deal with messages between users
  */
class UserActor extends Actor {
  // Information about the user
  private var userName: String = _
  private var userId: long = _
  private var fullName: String = _
  
  // the list of the latest messages for the user
  private var latestMsgs: List[Message] = Nil

  // The timeline for the current user
  private var localTimeline: List[Message] = Nil
  
  // the folks who are following this user 
  private var followers: List[Actor] = Nil
  
  // the listeners (either message listeners and/or listeners to the timeline)
  private var messageViewers: List[Actor] = Nil
  private var timelineViewers: List[Actor] = Nil

  /**
    * When the Actor is started, this method is invoked
    */
  def act = {
    this.trapExit = true // send messages on exit of linked Actors
    loop
  }
  
  /**
    * Sort the list in reverse chronological order and take the first 50 elements
    */
  private def merge(bigList: List[Message]) = bigList.sort((a,b) => b.when < a.when).take(50) 
  
  /**
    * Calculate a random persiod of at least 2 minutes and at most 8 minutes
    */
  private def randomPeriod: long = 2.minutes + randomLong(6.minutes)
  
  /**
    * The main loop that receives messages and processes them
    */
  private def loop {
    react {
      // The user sends a message containing text and a source.  This can
      // be from the web, from IM, from SMS, etc.
    case SendMessage(text, src) =>
    // create a new Message object to be added to the user's local message and sent
    // to followers
    val msg = Message(text, System.currentTimeMillis, userName, src)
    // add to our local messages (keeping only the 50 most recent messages)
    latestMsgs = (msg :: latestMsgs).take(50)
    // update all our followers (and ourselves) with the message 
    (this :: followers).foreach(_ ! msg)
    // and put it in the database
    MsgStore.create.message(text).source(src).who(userId).save
    loop
    
    // someone is asking us for our messages
    case GetMessages =>
    reply(Messages(latestMsgs)) // send them back
    loop
    
    // someone is asking us for our timeline
    case GetTimeline =>
    reply(Timeline(localTimeline))  // send it back
    loop
    
    // someone wants to know our name... tell them
    case GetUserIdAndName =>
    reply(UserIdInfo(userId, userName, fullName))
    loop
    
    // shut the user down
    case Bye =>
    UserList.remove(userName)
    this.exit(Bye)
    loop
    
    // set up the user
    case Setup(id, name, full) =>
    userId = id
    userName = name
    fullName = full
    UserList.add(userName, this) // add the user to the list
    // get the latest messages from the database
    latestMsgs = MsgStore.findAll(By(MsgStore.who, userId), 
                                  OrderBy(MsgStore.when, false),
                                  MaxRows(50)).map(s => Message(s.message, s.when, userName, s.source))
    localTimeline = latestMsgs  // set the local timeline to our messages (the folks we follow will update us)
    loop
    
    // Configure our followers by getting a list of followers
    // and sending them a list of our messages
    case ConfigFollowers =>
    // find all the followers
    followers = User.findAllByInsecureSql("SELECT users.* FROM users, friends WHERE users.id = friends.owner AND friends.friend = "+userId,
        true).
      flatMap(u => UserList.find(u.name).toList) // and get the Actor for the user
      
    // send them each a message to merge into the timeline
    followers.foreach(_ ! MergeIntoTimeline(latestMsgs))
    loop
    
    // if we add a friend, 
    case AddFriend(name) =>
    // find the user
    UserList.find(name).foreach{
      ua =>
        ua ! AddFollower // tell him we're a follower
        (ua !? GetUserIdAndName) match { // get the user info
          case UserIdInfo(id, _,_) => Friend.create.owner(userId).friend(id).save // and persist a friend connection in the DB
          case _ =>
        }
        }
    loop
    
    // We are removing a friend
    case RemoveFriend(name) =>
    // find the user
    UserList.find(name).foreach{
      ua =>
      ua ! RemoveFollower // tell them we're no longer following
      (ua !? GetUserIdAndName) match { // delete from database
        case UserIdInfo(id, _,_) => Friend.findAll(By(Friend.owner, userId), By(Friend.friend, id)).foreach(_.delete_!)
        case _ => 
      }
      }
    // remove from local timeline
    localTimeline = localTimeline.filter(_.who != name)
    
    // update timeline vieweres with the former-friend-free timeline
    timelineViewers.foreach(_ ! Timeline(localTimeline))
    loop
    
    // merge the messages (from a friend) into our local timeline
    case MergeIntoTimeline(msg) =>
    localTimeline = merge(localTimeline ::: msg)
    loop
    
    // add someone who is watching the timeline.  This Actor will get updates each time
    // the local timeline updates.  We link to them so we can remove them if they exit
    case AddTimelineViewer =>
    timelineViewers = sender :: timelineViewers
    this.link(sender)
    loop
    
    // remove the timeline viewer
    case RemoveTimelineViewer =>
    timelineViewers = timelineViewers.remove(_ == sender)
    this.unlink(sender)
    loop

    // Add an Actor to the list of folks who want to see when we get a message
    // this might be an IM or SMS output
    case AddMessageViewer =>
    messageViewers = sender :: messageViewers
    this.link(sender)
    loop
    
    // removes the message viewer
    case RemoveMessageViewer =>
    messageViewers = messageViewers.remove(_ == sender)
    this.unlink(sender)
    loop
        
    // add someone who is following us
    case AddFollower =>
    followers = sender :: followers // merge it in
    sender ! MergeIntoTimeline(latestMsgs) // give the follower our messages to merge into his timeline
    loop

    // remove the follower
    case RemoveFollower =>
    // filter out the sender of the message
    followers = followers.remove(_ == sender)
    loop

    // We get a message
    case msg : Message =>
    messageViewers.foreach(_ ! Messages(latestMsgs)) // send it to the message viewers
    localTimeline = (msg :: localTimeline).take(50) // update our timeline
    timelineViewers.foreach(_ ! Timeline(localTimeline)) // send the updated timeline to the timeline viewers
    loop

    // If someone is exiting, remove them from our lists
    case ('EXIT, who : Actor, why) =>
    messageViewers = messageViewers.remove(_ == who)
    timelineViewers = timelineViewers.remove(_ == who)
    Console.println(why)
    loop
    
    case s =>
    Console.println("User "+userName+" Got msg "+s)
    loop
  }
  }
}

/**
  * A singleton the holds the map between user names and Actors that service
  * the users.  Right now, this assumes that the Actors are all local, but 
  * it could also be extended to choose other machines (remote actors)
  * by using a hash on the username to choose the machine that's hosting
  * the user
  */
object UserList {
  private val set = new HashMap[String, UserActor]() // a map between the username and the Actor
  private val rwl = new ReentrantReadWriteLock // lots of readers, few writers
  private val r = rwl.readLock
  private val w = rwl.writeLock
  
  /**
    * Load all the users from the database and create actors for each of them
    */
  def create {
      // load all the users
      User.findAll.map{u =>
      val ua = new UserActor // create a new Actor
      ua.start // start it up
      ua ! Setup(u.id, u.name, u.wholeName) // tell it to set up
      ua // return it 
      }.foreach (_ ! ConfigFollowers) // for each of the UserActors, tell them to configure their followers
  }
  
  // We've just added a new user to the system
  // add that user to the list
  def startUser(who: User) {
    val ua = new UserActor
    ua.start
    ua ! Setup(who.id, who.name, who.wholeName)
    ua ! ConfigFollowers
  }
  
  def shutdown = foreach(_ ! Bye) // shutdown by telling each of the Actors a "Bye" message
  
  // iterate over all the actors in the system
  // and perform a function
  def foreach(f: UserActor => Any) {
    r.lock
    try {
    set.foreach(i => f(i._2))
    } finally {
      r.unlock
    }
  }
  
  // add a user to the list by mapping
  // the name to the UserActor
  def add(name: String, who: UserActor) {
    w.lock // lock
    try {
    set(name) = who // save
    } finally {
      w.unlock // unlock
    }
  }
  
  // find a user by name
  def find(name: String): Option[UserActor] = {
    r.lock
    try {
    set.get(name)
    } finally {
      r.unlock
    }
  }
  
  // remove a user
  def remove(name: String) {
    w.lock
    try {
    set -= name
    } finally {
      w.unlock
    }
  }
  
  def randomUsers = {
    r.lock
    try {
    for (val u <- set;
         shouldShow(1)) yield u._1
    } finally {
      r.unlock
    }
  }
  
}

/**
  * These are the messages that can be sent to (or from) a UserActor
  */
sealed abstract class UserMsg

/**
  * Send a message to a User (from the web, from IM, from SMS).  The Actor
  * will take care of persisting the information in the database.
  *
  * @param text - the text of the message
  * @param src - the source of the message
  */
case class SendMessage(text: String, src: String) extends UserMsg

/**
  * A message 
  * @param text - the text of the message
  * @param when - when was the message processed by the user object (about the time it was sent)
  * @param who - who sent the message (the user name)
  * @param src - how was the message sent
  */
case class Message(text: String, when: long, who: String, src: String) extends UserMsg

/**
  * Tell the UserActor to set itself up with the given user id, name, and full name
  * @param userId - the primary key of the user in the database
  * @param userName - the name of the user (e.g., john)
  * @param fullName - the first and last name of the user "John Q. Public"
  */
case class Setup(userId: long, userName: String, fullName: String) extends UserMsg

/**
  * Add a timeline viewer
  */
case object AddTimelineViewer extends UserMsg

/**
  * Remove a timeline viewer
  */
case object RemoveTimelineViewer extends UserMsg

/**
  * Add a message viewer
  */
case object AddMessageViewer extends UserMsg

/**
  * Remove a message viewer
  */
case object RemoveMessageViewer extends UserMsg

/**
  * Add a follower to this User
  */
case object AddFollower extends UserMsg

/**
  * Remove a follower
  */
case object RemoveFollower extends UserMsg

/**
  * Sent from a user to a follower telling the follower to merge the user's messages
  * into the followers timeline
  * @param what the messages to merge into the timeline
  */
case class MergeIntoTimeline(what: List[Message]) extends UserMsg

/**
  * Get the messages from the user
  */
case object GetMessages extends UserMsg

/**
  * Get the timeline from the user
  */
case object GetTimeline extends UserMsg

/**
  * Tell the user to gracefully shut itself down
  */
case object Bye extends UserMsg

/**
  * Tell the user to load a list of followers and and send them the user's messages to merge
  * into the follower's timeline
  */
case object ConfigFollowers extends UserMsg

/**
  * Ask the user for the userId, the name, and the fullName.  The user will
  * reply with a UserIdInfo message
  */
case object GetUserIdAndName extends UserMsg

/**
  * Send the current timeline
  * @param message - the local timeline for the user
  */
case class Timeline(messages: List[Message]) extends UserMsg

/**
  * The current messages for the user
  * @param messages a list of messages for the user
  */
case class Messages(messages: List[Message]) extends UserMsg

/**
  * Add a friend to the current user and persist the information
  * in the database.  
  * @param name the name of the user who is our new friend
  */
case class AddFriend(name: String) extends UserMsg

/**
  * Remove a friend and update the database.
  * @param name the name of the friend to remove
  */
case class RemoveFriend(name: String) extends UserMsg

/**
  * Returned from GetUserIdAndName
  * @param id the id/primary key of the user
  * @param name the user name
  * @param fullName the full name of the user 
  */
case class UserIdInfo(id: long, name: String, fullName: String)
