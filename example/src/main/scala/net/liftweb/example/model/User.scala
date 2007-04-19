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
  val textArea =  new MappedTextarea(this) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
}

/**
 * Fix the database
 */
/*
   object FixUp {
  def insureWeHaveATable {
    DB.use {
      db =>

	// get all the tables from the database
	val rs = db.getMetaData.getTables(null,null, null, null)
      var tables: List[String] = Nil
      while (rs.next) {tables = rs.getString(3).toLowerCase :: tables}
      
      // make sure we have a 'users' table
      val hasTable = tables.contains("users")
      rs.close
      
      // if we don't have the table, create it
      if (!hasTable) {
        val st = db.createStatement
        // create the table
        st.execute("CREATE TABLE users (id INT NOT NULL GENERATED ALWAYS AS IDENTITY, firstname VARCHAR(64), lastname VARCHAR(64), email VARCHAR(64), password_slt VARCHAR(64), password_pw VARCHAR(64), textarea LONG VARCHAR)")
        st.close
        
        // create a user
        val u = new User
        
        // populate the fields
        u.firstName := "Test"
        u.lastName := "User"
        u.email := "test@nodomain.net"
        u.password := "fruitbat"
        u.textArea := "Nothing much to say"
        
        // save it
        u.save
      }
      db.commit
    }
  }

}
*/