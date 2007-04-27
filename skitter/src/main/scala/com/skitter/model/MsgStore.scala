package com.skitter.model

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import DB._
import java.sql.Connection

/**
 * The singleton that has methods for accessing the database
 */
object MsgStore extends MsgStore with KeyedMetaMapper[long, MsgStore] {
  override def dbTableName = "messages" // define the DB table name
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class MsgStore extends KeyedMapper[long, MsgStore] {
  def getSingleton = MsgStore // what's the "meta" server
  def primaryKeyField = id
  
  val id = new MappedLongIndex(this)
  
  val message = new MappedString(this){ override def maxLen = 200}
  val who = new MappedLongForeignKey(this, User)
  val when = new MappedLong(this) {
    override def dbColumnName = "when_c"
    override def defaultValue = System.currentTimeMillis
  }
  val source = new MappedString(this)
}
