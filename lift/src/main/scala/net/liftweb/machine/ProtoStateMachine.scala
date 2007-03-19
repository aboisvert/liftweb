package net.liftweb.machine;

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import net.liftweb.proto._
import net.liftweb.util.Helpers._

/**
 *  This trait manages state/workflow transition 
 */
trait ProtoStateMachine[MyType <: ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType], 
			OtherType <: KeyedMapper[OtherKeyType, OtherType], 
			OtherKeyType,
			StateType <: Enumeration] extends KeyedMapper[long, MyType] 
{
  // the primary key for the database
  val id = new MappedLongIndex[MyType](this)
  override def primaryKeyField = id
  def getSingleton: MetaProtoStateMachine[MyType, OtherType, OtherKeyType, StateType]
  val currentState = new MappedInt[MyType](this)
  val timedEventAt = new MappedDateTime[MyType](this)

}

trait MetaProtoStateMachine [MyType <: ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType], 
                             OtherType <: KeyedMapper[OtherKeyType, OtherType], 
                             OtherKeyType,
                             StateType <: Enumeration] extends KeyedMetaMapper[long, MyType] with ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType] {
  def managedMetaMapper: KeyedMetaMapper[OtherKeyType, OtherType]
  val states : List[State]  
  def initialState : StateType#Value
  
  class State
  
  object State {
    def apply(name: StV, trans: Transition*) = new State
  }
  
  abstract class Transition {
    private var _action: Option[(ActionType, StV, StV, Event) => Any] = None  
    private var _guard: Option[(ActionType, StV, StV) => boolean] = None
    def action(act: (ActionType, StV, StV, Event) => Any): this.type = {_action = Some(act); this}
    def guard(gurd: (ActionType, StV, StV) => boolean): this.type = {_guard = Some(gurd); this}

  }
  
  // case class TimeTransition(to: StV, time: TimeSpan) extends Transition
  object TimeTransition {
    def apply(to: StV, when: TimeSpan): EventTransition = EventTransition(to) on {case TimerEvent(_) => true}
  }
                                                  
  case class EventTransition(to: StV) extends Transition {
    private var _on: Option[PartialFunction[Event, Any]] = None
    def on(why: PartialFunction[Event, Any]): this.type = {_on = Some(why); this}
  }
  
  // case 
  /*object Transition {
    def apply(to: StV) = new Transition
    def apply(to: StV, time: TimeSpan) = new Transition
    def apply(to: StV, time: TimeSpan, action: (ActionType, StV, StV) => unit) = new Transition
    def apply(to: StV, event: PartialFunction[Event, Any]) = new Transition
    // def apply[N](to: StV, event: PartialFunction[Event, N], action: (ActionType, StV, StV, N) => unit) = new Transition
  }*/
  
  abstract class Event
  case class TimerEvent(len: TimeSpan) extends Event
  
  type StV = StateType#Value
  type ActionType = ProtoStateMachine[MyType, OtherType, OtherKeyType, StateType]
}