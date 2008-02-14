/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.comet

import scala.actors.Actor
import scala.actors.Actor._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import scala.xml.{NodeSeq}
import scala.collection.immutable.TreeMap
import net.liftweb.textile.TextileParser
import scala.xml.Text
import java.util.Date

/**
 * A chat server.  It gets messages and returns them
 */

class ChatServer extends Actor {
  def act = loop(Nil, Nil)

  /**
    * Convert an incoming string into XHTML using Textile Markup
    *
    * @param msg the incoming string
    *
    * @return textile markup for the incoming string
    */
  def toHtml(msg: String): NodeSeq = TextileParser.parse(msg, Empty). // parse it
      map(_.toHtml.toList match {case Nil => Nil case x :: xs => x.child}).  // convert to html and get the first child (to avoid things being wrapped in <p>)
      getOrElse(Text(msg)) // if it wasn't parsable, then just return a Text node of the message

  def loop(chat: List[ChatLine], sessions: List[Actor]) {
    react {
    case ChatServerMsg(user, msg) if msg.length > 0 =>
      val chatu = (ChatLine(user, toHtml(msg), timeNow) :: chat).take(500)
      val toDistribute = chatu.take(15)
      sessions.foreach (_ ! ChatServerUpdate(toDistribute))
      loop(chatu, sessions)

    case ChatServerAdd(me) =>
      me ! ChatServerUpdate(chat.take(15))
      loop(chat, me :: sessions)

    case ChatServerRemove(me) => loop(chat, sessions.remove(_ == me))

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

case class ChatLine(user: String, msg: NodeSeq, when: Date)
case class ChatServerMsg(user: String, msg: String)
case class ChatServerUpdate(msgs: List[ChatLine])
case class ChatServerAdd(me: Actor)
case class ChatServerRemove(me: Actor)

