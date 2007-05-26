package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._

trait ProtoUser[T <: ProtoUser[T]] extends KeyedMapper[long, T] {
  // the primary key for the database
  object id extends MappedLongIndex[T](this)
  
  // First Name
  object firstName extends MappedString[T](this, 32)

  // Last Name
  object lastName extends MappedString[T](this, 32)

  // Email
  object email extends MappedEmail[T](this, 48)

  // Password
  object password extends MappedPassword[T](this)
}
