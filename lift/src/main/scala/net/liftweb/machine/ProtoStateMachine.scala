package net.liftweb.machine;

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import net.liftweb.proto._
import net.liftweb.util.Helpers._
import scala.collection.mutable.HashMap

/**
 *  This trait manages state/workflow transition 
 */
trait ProtoStateMachine[MyType <: ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType], 
			OtherType <: KeyedMapper[OtherKeyType, OtherType], 
			OtherKeyType,
			StateType <: Enumeration] extends KeyedMapper[long, MyType] 
{
  /**
    * Shorthand for one of the states
    */
  type StV = StateType#Value
  
  /**
    *  Shorthand for the meta state machine
    */
  type Meta = MetaProtoStateMachine[MyType, OtherType, OtherKeyType, StateType]

  /**
    * the primary key for the database
    */
  val id = new MappedLongIndex[MyType](this)
  
  /**
    * get the primary key field
    */
  override def primaryKeyField = id
  
  /**
    * Implement a method that returns the singleton
    */
  def getSingleton: Meta
  
  /**
    * The column in the database that stores the current state
    */
  val currentState = new MappedInt[MyType](this)
  
  /**
    * The column in the database that stores the next time an event should go off
    */
  val timedEventAt = new MappedDateTime[MyType](this)
  
  /**
    * Get the current state
    */
  def state: StateType#Value = getSingleton.stateEnumeration(currentState.get)
  
  /**
    * Set the current state
    */
  def state_=(what: StateType#Value): StateType#Value = {
      this.currentState := what.id
      what
    }
  
  /**
    * This method is called on a transition from one state to another.  Override this method
    * to perform an action.  Please call super to actually change the state and save the instance 
    */
  def transition(from: StV, to: StV, why: Meta#Event): unit = {
    this.state = to
    this.save
  }
  
  /**
    * This item has reached a terminating state.  This method will remove the
    * item from the database.  Override this method (please call super at the end of your method)
    * to do any cleanup.
    */
  def terminate(from: StV,to: StV,event: Meta#Event): unit = {
    this.delete_!
  }
    
  /**
    * Process an event
    */
  def processEvent(event: Meta#Event):unit = getSingleton.processEvent(this, event)
}

/**
  * A singleton that implements this trait will manage transitions, etc. for the state machine instance
  */
trait MetaProtoStateMachine [MyType <: ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType], 
                             OtherType <: KeyedMapper[OtherKeyType, OtherType], 
                             OtherKeyType,
                             StateType <: Enumeration] extends KeyedMetaMapper[long, MyType] with ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType] {
    
  /**
    * 
    */
//  def managedMetaMapper: KeyedMetaMapper[OtherKeyType, OtherType]

  /**
    * This method must be implemented.  It defines the states and legal state transitions
    */
  protected def states : List[State]  
                              
  /**
    * The default initial state
    */
  def initialState : StV
  
  /**
    * The enumeration of states
    */
  def stateEnumeration: StateType
  
  /**
    *  Terminate an instance
    */
  def terminate(what: MyType,from: StV,to: StV,event: Meta#Event) {what.terminate(from, to, event)}
  
  /**
    * An unhandled event has occurred.  By default, throw an exception.  However,
    * you can override this method (and not call "super") to log issues or do
    * something else
    */
  def unmatchedEventHanlder(who: MyType, what: Meta#Event) {
    throw new UnmatchedEventException("Event "+what+" was not matched at state "+who.state, who, what)
  }
  
  /**
    *  Process an event for an instance
    */
  def processEvent(who: MyType, what: Meta#Event) {
    val transitions = stateInfo(who.state)
    val which = first(transitions) {t => if (t.on.isDefinedAt(what) && t.testGuard(who, who.state, t.to, what)) Some(t) else None}
    if (!which.isDefined) unmatchedEventHanlder(who, what)
    which.foreach {
      t =>
      val to = t.to
      val old = who.state
      stateList.get(old).foreach(_.foreach(_.performExit(who, old, to, what)))
      t.performAction(who, old, to, what)
      who.transition(old, to, what)
      stateList.get(to).foreach(_.foreach(_.performEntry(who, old, to, what)))
    }
  }
  
  /**
    * Process an event for a given 
    */
  /*
  def processEvent(who: Any, what: Meta#Event): Option[MyType] = {
    val ret = find(who)
    ret.foreach(inst => processEvent(inst, what))
    ret
  }
  */
    
   // the state transition table
  private val stateInfo = new HashMap[StV, Seq[ATransition]]
  private val stateList = new HashMap[StV, Seq[State]]
                                      
  // process the states
  states.foreach {
    st =>

    stateInfo(st.name) = (stateInfo.get(st.name) getOrElse Nil ++ st.trans)
    stateList(st.name) = ((stateList.get(st.name) getOrElse Nil) ++ List(st))
  }
                       
  class State(val name: StV,val trans: Seq[ATransition]) {
    def entry(act: (MyType, StV, StV, Meta#Event) => Any): State = {this}
    def exit(act: (MyType, StV, StV, Meta#Event) => Any): State = {this}
    private var _entry: Option[(MyType, StV, StV, Meta#Event) => Any] = None
    private var _exit: Option[(MyType, StV, StV, Meta#Event) => Any] = None
    
    def performEntry(who: MyType, from: StV, to: StV, why: Meta#Event) {_entry.foreach(e => e(who, from, to, why))}
    def performExit(who: MyType, from: StV, to: StV, why: Meta#Event) {_exit.foreach(e => e(who, from, to, why))}
  }
  
  object State {
    def apply(name: StV, trans: ATransition*) = new State(name, trans)
  }
  
  abstract class ATransition(val to: StV,val on: PartialFunction[Meta#Event, Any]) {
    def testGuard(who: MyType, from: StV, to: StV, what: Meta#Event): boolean =
      _guard.map(g => g(who, from, to, what)) getOrElse true
      
    def performAction(who: MyType, from: StV, to: StV, what: Meta#Event) {
      _action.foreach(a => a(who, from, to, what))
    }
      
    private var _action: Option[(MyType, StV, StV, Meta#Event) => Any] = None  
    private var _guard: Option[(MyType, StV, StV, Meta#Event) => boolean] = None
    def action(act: (MyType, StV, StV, Meta#Event) => Any): this.type = {_action = Some(act); this}
    def guard(gurd: (MyType, StV, StV, Meta#Event) => boolean): this.type = {_guard = Some(gurd); this}

  }
  
  // case class TimeTransition(to: StV, time: TimeSpan) extends Transition
  def Timer(to: StV, when: TimeSpan): To = To(to, {case TimerEvent(_) => true})
  case class TimerEvent(len: TimeSpan) extends Event
  case class FirstTransition extends Event
                                                  
  case class To(override val to: StV,override val on: PartialFunction[Meta#Event, Any]) extends ATransition(to, on)
  
  abstract class Event
  
  class UnmatchedEventException(val msg: String, who: MyType,
                                    what: Meta#Event)  extends Exception(msg)
}

