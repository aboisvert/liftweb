package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import net.liftweb.mapper._
import net.liftweb.util.Helpers._

class MappedUniqueId[T<:Mapper[T]](owner : T, maxLen: Int) extends MappedString[T](owner, maxLen) {
  override def writePermission_? = false
  override lazy val defaultValue = randomString(maxLen)
  
  def reset(): T = this(randomString(maxLen))
}
