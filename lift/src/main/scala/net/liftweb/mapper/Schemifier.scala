package net.liftweb.mapper

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import java.sql._  

/**
  * Given a list of MetaMappers, make sure the database has the right schema
  * <ul>
  * <li>Make sure all the tables exists</li>
  * <li>Make sure the columns in the tables are correct</li>
  * <li>Create the indexes</li>
  * <li>Create the foreign keys</li>
  * </ul>
  */
object Schemifier {
  def schemify[T <: Mapper[T]](tables: List[MetaMapper[T]]) {
    DB.use {
      connection =>
      tables.foreach(t => ensureTable(t, connection))
      tables.foreach(t => ensureColumns(t, connection))
      tables.foreach(t => ensureIndexes(t, connection))
      tables.foreach(t => ensureConstraints(t, connection))
    }
  }
  
  private def ensureTable[T <: Mapper[T]](table: MetaMapper[T], connection: Connection) {
    
  }

  private def ensureColumns[T <: Mapper[T]](table: MetaMapper[T], connection: Connection) {
    
  }

  private def ensureIndexes[T <: Mapper[T]](table: MetaMapper[T], connection: Connection) {
    
  }

  private def ensureConstraints[T <: Mapper[T]](table: MetaMapper[T], connection: Connection) {
    
  }
}
