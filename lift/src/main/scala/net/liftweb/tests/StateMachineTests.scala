package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.testing.SUnit
import SUnit._

import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.mapper._
import net.liftweb.proto._
import net.liftweb.machine._

class StateMachineTests extends TestCase("State Machine Tests") {
  override def runTest() {
    try {
    TestStateMachine.woofer
    } catch {
      case e => e.printStackTrace
    }
  }
}

object TestStateMachine extends TestStateMachine with MetaProtoStateMachine[TestStateMachine, User, long, Moo] {
  def managedMetaMapper = User
  
  case class TestEvent1 extends Event
  case class TestEvent2 extends Event
  
  val woofer = 44
  
  def initialState = Moo.One
  protected def states = {
    // val t: PartialFunction[Event, int] = {case TestEvent1() => 33}
    State(Moo.One, Timer(Moo.Two, 10 seconds) action ((obj, from, to, event) => {from.id == to.id; obj.woof}),
                   To(Moo.Three, {case TestEvent1() => }) action ((obj, from, to, event) => false) ) ::
    State(Moo.Two, To(Moo.One, {case TestEvent2() => }) guard ((obj, from, to, event) => false)) ::
               Nil
  }
}

object Moo extends Moo {
  val One, Two, Three = Value
}


class Moo extends Enumeration {
  
}

class TestStateMachine extends ProtoStateMachine[TestStateMachine, User, long, Moo] {
  def getSingleton = TestStateMachine
  def woof = "Hello"
}
