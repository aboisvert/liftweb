/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.example.model

import _root_.net.liftweb.mapper._
import DB._
import _root_.java.sql.Connection

/**
 * The singleton that has methods for accessing the database
 */
trait MetaWikiEntry extends WikiEntry with KeyedMetaMapper[long, WikiEntry]
object WikiEntry extends MetaWikiEntry {
  override def dbTableName = "wikientry" // define the DB table name

  // define the order fields will appear in forms and output
  override def fieldOrder =  id :: name :: entry :: Nil
}

/**
 * An O-R mapped wiki entry
 */
class WikiEntry extends KeyedMapper[Long, WikiEntry] {
  def getSingleton = WikiEntry // what's the "meta" object
  def primaryKeyField = id

  // the primary key
  object id extends MappedLongIndex(this)

  // the name of the entry
  object name extends MappedString(this, 32) {
    override def dbIndexed_? = true // indexed in the DB
  }

  // the text of the entry
  object entry extends MappedTextarea(this, 8192) {
    override def textareaRows  = 10
    override def textareaCols = 50
  }
}
