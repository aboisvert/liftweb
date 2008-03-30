/*
* Copyright 2008 WorldWide Conferencing, LLC
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
package net.liftweb.example.snippet

import net.liftweb.example.model._
import scala.xml.{NodeSeq, Text, Group, Node}
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.util._

/**
  * This snippet handles counting
  */
class Database {
  
  /**
    * This method is invoked by the &lt;lift:Count /&gt; tag
    */
  def render(in: NodeSeq): NodeSeq = {
	val count = Person.count()
	val first = Person.find(OrderBy(Person.firstName, true), MaxRows[Person](1))
	
	bind("database", in, "count" --> count,
	"first" --> first.map(_.asHtml).openOr(<b>No Persons in the system</b>),
	"submit" --> submit("Create More Records", ignore => {
		val cnt = 10 + randomInt(50)
		for (x <- 1 to cnt) Person.create.firstName(randomString(20)).lastName(randomString(20)).personalityType(Personality.rand).save
		notice("Added "+cnt+" records to the Person table")
	}))
  }
}
