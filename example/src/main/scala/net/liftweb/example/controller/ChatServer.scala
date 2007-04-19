package net.liftweb.example.controller

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.actors.Actor
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import scala.xml.{NodeSeq}
import scala.collection.immutable.TreeMap
import java.util.Date

/**
 * A chat server.  It gets messages and returns them
 */

class ChatServer extends Actor {
  def act = loop(Nil, Nil)
  
  def loop(chat: List[ChatLine], sessions: List[Actor]) {
    react {
    case ChatServerMsg(user, msg) => {
      val chatu = (ChatLine(user, msg, new Date) :: chat).take(500)
      val toDistribute = chatu.take(15)
      sessions.foreach {a => a ! ChatServerUpdate(toDistribute)}
      loop(chatu, sessions)
    }
    case ChatServerAdd(me) => {
      reply(ChatServerUpdate(chat.take(15)))
      loop(chat, me :: sessions)
    }
    case ChatServerRemove(me) => {
      loop(chat, sessions.remove{e => e == me})
    }
    case _ => loop(chat, sessions)
  }
  }
}

object ChatServer {
  val server = {
    val ret = new ChatServer
    ret.start
    ret
  }
}

case class ChatLine(user: String, msg: String, when: Date)
case class ChatServerMsg(user: String, msg: String)
case class ChatServerUpdate(msgs: List[ChatLine])
case class ChatServerAdd(me: Actor)
case class ChatServerRemove(me: Actor)

