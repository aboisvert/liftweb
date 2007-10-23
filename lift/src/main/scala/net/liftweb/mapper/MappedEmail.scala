package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                */

import java.util.regex._

object MappedEmail {
  val emailPattern = Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
  def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches
}

class MappedEmail[T<:Mapper[T]](owner: T, maxLen: Int) extends MappedString[T](owner, maxLen) {

  override def setFilter = notNull _ :: toLower _ :: trim _ :: super.setFilter 
    
  override def validate =
    (if (MappedEmail.emailPattern.matcher(i_is_!).matches) Nil else List(ValidationIssue(this, "Invalid Email Address"))) :::
    super.validate

}
