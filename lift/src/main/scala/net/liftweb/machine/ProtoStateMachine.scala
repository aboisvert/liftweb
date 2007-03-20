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
  type StV = StateType#Value

  class State(val name: StV,val trans: Seq[ATransition])
  
  object State {
    def apply(name: StV, trans: ATransition*) = new State(name, trans)
  }
  
  abstract class ATransition(val to: StV,val on: PartialFunction[Event, Any]) {
    private var _action: Option[(MyType, StV, StV, Event) => Any] = None  
    private var _guard: Option[(MyType, StV, StV, Event) => boolean] = None
    def action(act: (MyType, StV, StV, Event) => Any): this.type = {_action = Some(act); this}
    def guard(gurd: (MyType, StV, StV, Event) => boolean): this.type = {_guard = Some(gurd); this}

  }
  
  // case class TimeTransition(to: StV, time: TimeSpan) extends Transition
  object Timer {
    def apply(to: StV, when: TimeSpan): To = To(to, {case TimerEvent(_) => true})
  }
                                                  
  case class To(override val to: StV,override val on: PartialFunction[Event, Any]) extends ATransition(to, on)
  
  abstract class Event
  case class TimerEvent(len: TimeSpan) extends Event
  
  // the primary key for the database
  val id = new MappedLongIndex[MyType](this)
  override def primaryKeyField = id
  def getSingleton: MetaProtoStateMachine[MyType, OtherType, OtherKeyType, StateType]
  val currentState = new MappedInt[MyType](this)
  val timedEventAt = new MappedDateTime[MyType](this)
  
  def transition(from: StV, to: StV, why: Event): Any = {}
  def terminate(from: StV,to: StV,event: MyType#Event) {}
}

trait MetaProtoStateMachine [MyType <: ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType], 
                             OtherType <: KeyedMapper[OtherKeyType, OtherType], 
                             OtherKeyType,
                             StateType <: Enumeration] extends KeyedMetaMapper[long, MyType] with ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType] {
  def managedMetaMapper: KeyedMetaMapper[OtherKeyType, OtherType]
  protected def states : List[State]  
  def initialState : StV
  
  def terminate(what: MyType,from: StV,to: StV,event: MyType#Event) {what.terminate(from, to, event)}
  
  
  private val stateInfo = new HashMap[StV, List[ATransition]]
                                      
  states.foreach {
    st =>
    val cur = stateInfo.get(st.name) getOrElse Nil
    Console.println("State "+st.name+" trans "+st.trans)
    stateInfo(st.name) = (cur ++ st.trans).toList
  }
                                      
                                      
}