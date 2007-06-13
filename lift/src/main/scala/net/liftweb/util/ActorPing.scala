package net.liftweb.util

import scala.actors.Actor
import java.util.concurrent._

object ActorPing {
  def start: Unit = {}
  
  def snapshot: Unit = {}
  
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
           d setDaemon true
           d
   }
}

/*

/*                     __                                               *\
 **     ________ ___   / /  ___     Scala API                            **
 **    / __/ __// _ | / /  / _ |    (c) 2005-2007, LAMP/EPFL             **
 **  __\ \/ /__/ __ |/ /__/ __ |                                         **
 ** /____/\___/_/ |_/____/_/ | |                                         **
 **                          |/                                          **
 \*                                                                      */

import java.lang.{InterruptedException, Runnable, Thread}

import scala.collection.mutable.PriorityQueue
import scala.actors.Actor

/**
 * A derivative of the Scala actor TimerThread object that allows arbitrary messages
 * to be sent to Actors
 *
 * @version 0.9.2
 * @author Sebastien Noir, Philipp Haller, David Pollak
 */

object ActorPing extends AnyRef with Runnable {

  case class WakedActor(actor: Actor, msg: Any, time: long)
       extends Ordered[WakedActor] {
	 var valid = true
	 def compare(that: WakedActor): int = -(this.time compare that.time)
       }

  private var queue = new PriorityQueue[WakedActor]
  private var t = new Thread(this); t setDaemon true; t.start
  
  private var lateList: List[WakedActor] = Nil
  private var running = false

  /**
   * @param a ...
   */
  def trashRequest(a: Actor) = synchronized {
    // keep in mind: killing dead people is a bad idea!
    queue.elements.find((wa: WakedActor) => wa.actor == a && wa.valid) match {
      case Some(b) =>
      b.valid = false
      case None =>
        lateList.find((wa2: WakedActor) => wa2.actor == a && wa2.valid) match {
          case Some(b2) =>
          b2.valid = false
          case None =>
        }
    }
  }
  
  def start: unit = synchronized {
    if (!running) {
      running = true
      t = new Thread(this)
      t setDaemon true
      t.start
    }
  }
  
  def snapshot: unit = synchronized {
    if (running) {
      running = false
      notifyAll
    }
  }

  override def run {
    try {
      while(running) {
        this.synchronized {
            val sleepTime = dequeueLateAndGetSleepTime
            if (lateList.isEmpty) wait(sleepTime)

        // process guys waiting for signal and empty list
        for (wa <- lateList) {
          if (wa.valid) {
            wa.actor ! wa.msg
          }
        }
        lateList = Nil
        }
      }
    } catch {
      case consumed: InterruptedException => consumed.printStackTrace
      // allow thread to quit
    }
  }

  /**
   * @param a          ...
   * @param f          ...
   * @param waitMillis ...
   */
  def schedule(a: Actor, msg: Any, waitMillis: long): unit = synchronized {
    val wakeTime = now + waitMillis
    if (waitMillis <= 0) {
      return
    } 

    if (queue.isEmpty) { // add to queue and restart sleeping
      queue += WakedActor(a, msg, wakeTime)
      notify()
    } else
      if (queue.max.time > wakeTime) { // add to 1st position and restart sleeping
        queue += WakedActor (a, msg, wakeTime)
        notify()
      }
      else // simply add to queue
        queue += WakedActor (a, msg, wakeTime)
  }

  private def dequeueLateAndGetSleepTime: long = {
    val FOREVER: long = 0
    var waitingList: List[WakedActor] = Nil
    val now = this.now

    while (!queue.isEmpty) {
      val next = queue.max.time
      val amount = next - now
      if (amount > 0) { // guy in queue is not late
        lateList = waitingList // give back the list of waiting guys for signaling
        return amount
      }
      else // we're late: dequeue and examine next guy
        waitingList = queue.dequeue :: waitingList
    }

    // empty queue => sleep forever
    lateList = waitingList
    return FOREVER
  }

  def now = System.currentTimeMillis
}
*/
