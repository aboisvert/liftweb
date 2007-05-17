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
object User extends User with KeyedMetaMapper[long, User] {
  protected override def internalTableName_$ = "users" // define the DB table name
  
  // define the order fields will appear in forms and output
  override def fieldOrder = id :: firstName :: lastName :: email :: 
  password :: textArea :: Nil
  

}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends ProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  
  // define an additional field for a personal essay
  val textArea =  new MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}
