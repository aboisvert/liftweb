package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.actors.Actor
import java.util.concurrent._

object ActorPing {
  def start: Unit = {}
  
  def snapshot: Unit = {}
  
  def shutdown: Unit = { service.shutdown }

  def schedule(to: Actor, msg: Any, delay: Long): ScheduledFuture = schedule(to, msg, delay, TimeUnit.MILLISECONDS)
  
  // Can be canceled by using the future returned.
  def schedule(to: Actor, msg: Any, delay: Long, tu: TimeUnit) :
  ScheduledFuture = {
    val r = new Runnable { def run { to ! msg } }
    service.schedule(r, delay, tu)
  }
  
  private val service =
    Executors.newSingleThreadScheduledExecutor(TF)
}

private object TF extends ThreadFactory {
  val threadFactory = Executors.defaultThreadFactory()
  def newThread(r: Runnable) : Thread = {
    val d: Thread = threadFactory.newThread(r)
    d setName "ActorPing"
    d setDaemon true
    d
  }
}
