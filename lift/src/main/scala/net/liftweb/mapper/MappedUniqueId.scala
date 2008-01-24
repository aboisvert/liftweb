package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._
import net.liftweb.http.S
import scala.xml.{Elem, NodeSeq}

class MappedUniqueId[T<:Mapper[T]](owner : T, maxLen: Int) extends MappedString[T](owner, maxLen) {
  override def writePermission_? = false
  override lazy val defaultValue = randomString(maxLen)
  
  def reset(): T = this(randomString(maxLen))
}

/**
  * A field that holds the birth year for the user
  */
class MappedBirthYear[T <: Mapper[T]](owner: T, minAge: Int) extends MappedInt[T](owner) {
  override def defaultValue = year(timeNow) - minAge
  
  override def _toForm: Can[NodeSeq] = {
    val end = (year(timeNow) - minAge)
      val start = end - 100
    Full(S.select((start to end).
		  toList.
		  reverse.
		  map(y => (y.toString, y.toString)), 
		  Full(is.toString),v => set(v.toInt)))
  }
}

class MappedGender[T <: Mapper[T]](owner: T) extends MappedEnum(owner, Genders) {
  override def defaultValue = Genders.Male
}

object Genders extends Enumeration {
  val Male = Value(1, "Male")
  val Female = Value(2, "Female")
}
