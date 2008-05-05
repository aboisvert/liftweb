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

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import js._
import JsCmds._

class Clock (info: CometActorInitInfo) extends CometActor(info) {

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
