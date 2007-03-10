package net.liftweb.mapper

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import java.sql.Connection

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
    case class SuperConnection(connection: Connection) {
      val driverType = calcDriver(connection.getMetaData.getDatabaseProductName)
    }
    implicit def superToRegConnection(sc: SuperConnection): Connection = sc.connection
    
  def schemify[T <: Mapper[T]](tables: MetaMapper[T]*) {
    DB.use {
      con =>
      val connection = SuperConnection(con)
      val driver = calcDriver(connection.getMetaData.getDatabaseProductName)
      tables.foreach{t => ensureTable(t, connection )}
      tables.foreach{t => ensureColumns(t, connection)}
      tables.foreach{t => ensureIndexes(t, connection)}
      tables.foreach{t => ensureConstraints(t, connection)}
    }
  }
  
  private def calcDriver(name: String): DriverType = {
    name match {
      case "Apache Derby" => DerbyDriver
    }
  }
  
  private def ensureTable[T <: Mapper[T]](table: MetaMapper[T], connection: SuperConnection) {
    val md = connection.getMetaData
    val rs = md.getTables(null, null, null, null)
    var hasTable = false
    while (!hasTable && rs.next) {
      val tableName = rs.getString(3)
      hasTable = table.tableName_$.toLowerCase == tableName.toLowerCase
    }
    rs.close
    if (!hasTable) {
      val ct = "CREATE TABLE "+table.tableName_$+" ("+createColumns(table, connection).mkString("", " , ", "")+")";
      val st = connection.createStatement
      st.execute(ct)
      st.close
        
      Console.println(ct)
    }
  }
  
  private def createColumns[T <: Mapper[T]](table: MetaMapper[T], connection: SuperConnection): Seq[String] = {
    table.mappedFields.flatMap(_.fieldCreatorString(connection.driverType))
  }

  private def ensureColumns[T <: Mapper[T]](table: MetaMapper[T], connection: SuperConnection) {
    table.mappedFields.foreach {
      field =>
      var hasColumn = 0
      var cols: List[String] = Nil
      val totalColCnt = field.dbColumnCount
      val md = connection.getMetaData
      val rs = md.getColumns(null, null, null, null)
      while (hasColumn < totalColCnt && rs.next) {
        val tableName = rs.getString(3).toLowerCase
        val columnName = rs.getString(4).toLowerCase
        if (tableName == table.tableName_$ && field.dbColumnNames(field.name).contains(columnName)) {
          cols = columnName :: cols
          hasColumn = hasColumn + 1
        }
      }
      rs.close
      
      // FIXME deal with column types
      (field.dbColumnNames(field.name) diff cols).foreach {
        colName =>
        
        val ct = "ALTER TABLE "+table.tableName_$+" ADD COLUMN "+field.fieldCreatorString(connection.driverType, colName)
        val st = connection.createStatement
        st.execute(ct)
        st.close
      }
      
    }
      
  }

  private def ensureIndexes[T <: Mapper[T]](table: MetaMapper[T], connection: SuperConnection) {
    
  }

  private def ensureConstraints[T <: Mapper[T]](table: MetaMapper[T], connection: SuperConnection) {
    
  }
}
  
abstract class DriverType
object MySqlDriver extends DriverType
object DerbyDriver extends DriverType

