package com.hellolift.model

import net.liftweb.mapper._
import net.liftweb.util._

object Entry extends Entry with KeyedMetaMapper[Long, Entry] {
  override def dbTableName = "entries"
}

class Entry extends KeyedMapper[Long, Entry] {
  def getSingleton = Entry // what's the "meta" server
  def primaryKeyField = id
 
  // Fields
  object id extends MappedLongIndex(this)
  object author extends MappedLongForeignKey(this, User)
  object title extends MappedString(this, 128)
  object body extends MappedTextarea(this, 32000) { // Lift Note: 7
    override def setFilter = notNull _  :: trim _ :: crop _ :: super.setFilter
  }
}
