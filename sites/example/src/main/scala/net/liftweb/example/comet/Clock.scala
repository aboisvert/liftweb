package net.liftweb.example.comet

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._

class Clock (theSession: LiftSession, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]) extends 
      CometActor(theSession, name, defaultXml, attributes) {

  def defaultPrefix = "clk"
  ActorPing.schedule(this, Tick, 10000L) // schedule a ping every 10 seconds so we redraw
  
  def render = bind("time" -> Text(timeNow.toString))
  
  override def lowPriority : PartialFunction[Any, Unit] = {
    case Tick => 
    reRender // tell the component to redraw itself
    ActorPing.schedule(this, Tick, 10000L) // schedule an update in 10 seconds
  }
}

case object Tick