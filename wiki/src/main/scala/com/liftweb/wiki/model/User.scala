package com.liftweb.wiki.model

import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import java.util.regex._

class User extends KeyedMapper[long, User] {
  object id extends MappedLongIndex(this)
  object username extends MappedString(this, 100)
  object password extends MappedString(this, 100)

  def getSingleton = User
  def primaryKeyField = id
}

object User extends User with KeyedMetaMapper[long, User] {
  override def dbTableName = "users"

  def log(username: String, password: String) =
    find(By(User.username, username)) match {
      case Some(user) =>
        if (user.password.get == password) Some(user)
        else None
      case None =>
        None
    }
}
