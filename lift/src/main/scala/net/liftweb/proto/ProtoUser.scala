package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
  \*                                                 */

import net.liftweb.mapper._
 
trait ProtoUser[T <: ProtoUser[T]] extends Mapper[T] {
  // the primary key for the database
  val id = new MappedIntIndex(this)
  
  // First Name
  val firstName = new MappedString(this) {override def maxLen =32}

  // Last Name
  val lastName = new MappedString(this) {override def maxLen =32}

  // Email
  val email = new MappedEmail(this)

  // Password
  val password = new MappedPassword(this)
  
}
