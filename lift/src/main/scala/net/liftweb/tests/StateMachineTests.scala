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

object TestStateMachine extends TestStateMachine with MetaProtoStateMachine[TestStateMachine, User, long, TestState.type] {
  def managedMetaMapper = User
  
  val woofer = 44
  
  def initialState = TestState.Initial
  protected def states = {
    State(TestState.Initial, To(TestState.First, {case FirstTransition() => })) ::
    State(TestState.First, Timer(TestState.Second, 10 seconds) action ((obj, from, to, event) => {from.id == to.id; obj.woof}),
                   To(TestState.Third, {case TestEvent1() => }) action ((obj, from, to, event) => false) ) ::
    State(TestState.Second, To(TestState.First, {case TestEvent2() => }) guard ((obj, from, to, event) => false)) ::
    (State(TestState.Third) entry (terminate) exit (terminate)) ::
               Nil
  }
  def stateEnumeration = TestState
  
  protected def instantiate = new TestStateMachine

  case class FirstTransition extends Event
case class TestEvent1 extends Event
case class TestEvent2 extends Event
//case class TimerEvent(len: TimeSpan) extends Event
//def Timer(to: StV, when: TimeSpan): To = To(to, {case TimerEvent(_) => true})
}

object TestState extends Enumeration {
  val Initial, First, Second, Third = Value
}


class TestStateMachine extends ProtoStateMachine[TestStateMachine, User, long, TestState.type] {
  def getSingleton = TestStateMachine
  def woof = "Hello"


}
