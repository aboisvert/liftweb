package com.hellolift.model

import net.liftweb.mapper._
import net.liftweb.util._

object Blog extends Blog with KeyedMetaMapper[Long, Blog] {
}

class Blog extends KeyedMapper[Long, Blog] {
  def getSingleton = Blog // what's the "meta" server
  def primaryKeyField = id

  // Fields
  object id extends MappedLongIndex(this)
  object owner extends MappedLongForeignKey(this, User)
  object name extends MappedString(this, 64) {
    override def setFilter = notNull _  :: trim _ :: crop _ :: super.setFilter
  }
}

