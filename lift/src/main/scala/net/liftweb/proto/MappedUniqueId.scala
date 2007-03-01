package net.liftweb.proto

/*                                                *\
  (c) 2006-2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                */
  
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util.Lazy
import net.liftweb.util.Lazy._

class MappedUniqueId[T<:Mapper[T]](owner : Mapper[T]) extends MappedString[T](owner) {
  override def writePermission_? = false
  private val dv = Lazy{randomString(maxLen)}
  override def defaultValue = dv.get
}
