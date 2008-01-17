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
import js._
import JsCmds._

class Clock (theSession: LiftSession, name: Can[String], defaultXml: NodeSeq, attributes: Map[String, String]) extends CometActor(theSession, name, defaultXml, attributes) {
  
  def defaultPrefix = "clk"
  ActorPing.schedule(this, Tick, 10000L) // schedule a ping every 10 seconds so we redraw
  
  private lazy val spanId = uniqueId+"_timespan"

  def render = bind("time" -> timeSpan)

  def timeSpan = (<span id={spanId}>{timeNow}</span>)
  
  override def lowPriority : PartialFunction[Any, Unit] = {
    case Tick => 
      partialUpdate(SetHtml(spanId, Text(timeNow.toString)))
    ActorPing.schedule(this, Tick, 10000L) // schedule an update in 10 seconds
  }
}

case object Tick
