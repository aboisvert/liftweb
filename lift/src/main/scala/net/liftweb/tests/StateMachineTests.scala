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
    
  }
}

object TestStateMachine extends TestStateMachine with KeyedMetaMapper[long, TestStateMachine] {
  
}

object Moo extends Moo {
  
}

class Moo extends Enumeration {
  
}

class TestStateMachine extends ProtoStateMachine[TestStateMachine, User, long, Moo] {
  def getSingleton = TestStateMachine
  def managedMetaMapper = User
  
}
