package net.liftweb.machine;

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import net.liftweb.proto._

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
  val currentState = new MappedInt[MyType](this)
  val timedEventAt = new MappedDateTime[MyType](this)
  def managedMetaMapper: KeyedMetaMapper[OtherKeyType, OtherType]
  
}
