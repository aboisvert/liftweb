package net.liftweb.example.model

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
object WikiEntry extends WikiEntry with KeyedMetaMapper[long, WikiEntry] {
  protected override def internalTableName_$ = "wiki_entry" // define the DB table name
  
  // define the order fields will appear in forms and output
  override def fieldOrder =  id :: entry :: Nil
}

/**
 * An O-R mapped wiki entry
 */
class WikiEntry extends KeyedMapper[long, WikiEntry] {
  def getSingleton = WikiEntry // what's the "meta" object
  def primaryKeyField = id

  // the primary key
  val id = new MappedLongIndex(this)
  
  // the name of the entry
  val name = new MappedString(this) {
    override def dbIndexed_? = true // indexed in the DB
  }
  
  // the text of the entry
  val entry =  new MappedTextarea(this) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def maxLen = 8192
  }
}
