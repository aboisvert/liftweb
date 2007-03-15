package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._

trait ProtoUser[T <: ProtoUser[T]] extends KeyedMapper[long, T] {
  // the primary key for the database
  val id = new MappedLongIndex[T](this)
  
  // First Name
  val firstName = new MappedString[T](this) {override def maxLen =32}

  // Last Name
  val lastName = new MappedString[T](this) {override def maxLen =32}

  // Email
  val email = new MappedEmail[T](this)

  // Password
  val password = new MappedPassword[T](this)
  
}
