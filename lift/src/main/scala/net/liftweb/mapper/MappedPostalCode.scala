package net.liftweb.mapper

import scala.xml.{NodeSeq}
import net.liftweb.http.S
import net.liftweb.http.S._

class MappedPostalCode[T<:Mapper[T]](owner : T, country: MappedString[T]) extends MappedString[T](owner, 32) {

}
