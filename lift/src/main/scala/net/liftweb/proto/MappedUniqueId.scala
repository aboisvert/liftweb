package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */
  
import net.liftweb.mapper._
import net.liftweb.util.Helpers._

class MappedUniqueId[T](owner : Mapper[T]) extends MappedString[T](owner) {
  override def write_permission_? = false
  override def defaultValue : String = defaultValue_i
  
  private val defaultValue_i = randomString(maxLen)
}
