package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.util.regex._

object MappedEmail {
  val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
}

class MappedEmail[T<:Mapper[T]](owner : T) extends MappedString[T](owner) {

  override protected def i_set_!(value : String) : String = {
    super.i_set_!(value match {
      case null => ""
      case _ => value.toLowerCase.trim
    })
  }
  
  override def convertToJDBCFriendly(value: String): Object = value match {
    case null => ""
    case s => s.toLowerCase.trim
  }

  
  override def validate /*: List[ValidationIssues[String, T]] */ = {
    MappedEmail.emailPattern.matcher(i_get_!).matches match {
      case true => Nil
      case false => List(ValidationIssue(this, "Invalid Email Address"))
    }
  }
}
