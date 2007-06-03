package com.liftweb.wiki.model

import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import java.util.regex._

class Revision extends KeyedMapper[long, Revision] {
  object id extends MappedLongIndex(this)
  object text extends MappedTextarea(this, 1000)
  //object previous extends MappedLongForeignKey(this, Revision)
  object previousKey extends MappedLong(this)
  //object author extends MappedLongForeignKey(this, Person)
  object authorKey extends MappedLong(this)

  def previous =
    Revision.find(previousKey)

  def author =
    User.find(authorKey)

  def getSingleton = Revision
  def primaryKeyField = id
}

object Revision extends Revision with KeyedMetaMapper[long, Revision]
