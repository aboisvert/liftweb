/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.model

import net.liftweb.mapper._
import net.liftweb.util._
import DB._
import java.sql.Connection

object Person extends Person with KeyedMetaMapper[Long,Person] {

}

class Person extends KeyedMapper[Long,Person] {
 def getSingleton = Person
 def primaryKeyField = personId

 object personId extends MappedLongIndex(this)

 object firstName extends MappedString(this, 100)
 object lastName  extends MappedString(this, 100)

 object personalityType extends MappedEnum(this, Personality)
}

object Personality extends Enumeration {
	val TypeA = Value(1, "Type A")
	val TypeB = Value(2, "Type B")
	val ENTJ = Value(3, "ENTJ")
	val INTJ = Value(4, "INTJ")

	val allTypes = Array(TypeA, TypeB, ENTJ, INTJ)
	def rand = allTypes(Helpers.randomInt(allTypes.length))
}