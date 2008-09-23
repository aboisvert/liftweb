package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import _root_.scala.testing.SUnit
import SUnit._

import _root_.net.liftweb.util.{Helpers, Log}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.machine._

class StateMachineTests extends TestCase("State Machine Tests") {
  override def runTest() {

    TestStateMachine.didExitInitial = false
    TestStateMachine.didEnterFirst = false
    TestStateMachine.didExitFirst = false
    TestStateMachine.didEnterSecond = false

    val user = new User
    user.save
    TestStateMachine.stateEnumeration // cause the singleton to get loaded
    val tran = TestStateMachine.FirstTransition
    val toTest = TestStateMachine.createNewInstance(tran) {i => i.managedItem(user)}

    assert(TestStateMachine.didExitInitial)
    assert(TestStateMachine.didEnterFirst)
    assert(!TestStateMachine.didExitFirst)
    assert(!TestStateMachine.didEnterSecond)
    Thread.sleep(3000L)
    assert(TestStateMachine.didExitFirst)
    assert(TestStateMachine.didEnterSecond)

    val toTest2 = TestStateMachine.find(toTest.id).open_!
    assert(toTest2.state == TestState.Second)

    toTest2.processEvent(TestStateMachine.TestEvent2)
    assert(TestStateMachine.count == 0) // make sure the terminated request is removed from the DB
  }
}

object TestStateMachine extends TestStateMachine with MetaProtoStateMachine[TestStateMachine, TestState.type] {
  def managedMetaMapper = User

  var didExitInitial = false
  var didEnterFirst = false
  var didExitFirst = false
  var didEnterSecond = false

  def initialState = TestState.Initial
  protected def states = {
    (State(TestState.Initial, On({case FirstTransition => }, TestState.First)) exit {(a,b,c,d) => didExitInitial = true; true}) ::
    (State(TestState.First, After( 2 seconds, TestState.Second) action ((obj, from, to, event) => {from.id == to.id; obj.woof}),
           On({case TestEvent1 => }, TestState.Third) action ((obj, from, to, event) => false))
     exit {(a,b,c,d) => didExitFirst = true}
     entry {(a,b,c,d) => didEnterFirst = true}) ::
    (State(TestState.Second, On({case TestEvent2 => }, TestState.First ) guard ((obj, from, to, event) => false),
           On( {case TestEvent2 => }, TestState.Third) guard ((obj, from, to, event) => true)) entry {(a,b,c,d) => didEnterSecond = true}) ::
    (State(TestState.Third) entry (terminate) exit (terminate)) ::
    Nil
  }


  /**
   * Any transitions that are applied to all states can be listed here
   */
  protected override def globalTransitions: List[ATransition] = Nil

  def stateEnumeration = TestState

  protected def instantiate = new TestStateMachine

  /**
   * How long to wait to start looking for timed events.  Override this method to
   * specify a time
   */
  override def timedEventInitialWait = 1000L

  /**
   * After the initial test, how long do we wait
   */
  override def timedEventPeriodicWait = 200L

  case object FirstTransition extends Event
  case object TestEvent1 extends Event
  case object TestEvent2 extends Event
  //case class TimerEvent(len: TimeSpan) extends Event
  //def Timer(to: StV, when: TimeSpan): To = To(to, {case TimerEvent(_) => true})
}

object TestState extends Enumeration {
  val Initial, First, Second, Third = Value
}


class TestStateMachine extends ProtoStateMachine[TestStateMachine, TestState.type] {
  def getSingleton = TestStateMachine
  def woof = "Hello"


  /**
   * This method is called on a transition from one state to another.  Override this method
   * to perform an action.  Please call super to actually change the state and save the instance
   */
  override def transition(from: StV, to: StV, why: Meta#Event): Unit = {
    Log.debug("Transition from "+from+" to "+to+" why "+why+" at "+timeNow)
    super.transition(from, to, why)
  }


  object managedItem extends MappedLongForeignKey(this, User)
}
