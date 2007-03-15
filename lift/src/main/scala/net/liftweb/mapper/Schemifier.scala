package net.liftweb.mapper

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.sql.Connection
import scala.collection.mutable.HashMap

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
  
  def schemify(tables: BaseMetaMapper*) {
    DB.use {
      con =>
	val connection = SuperConnection(con)
      val driver = calcDriver(connection.getMetaData.getDatabaseProductName)
      val actualTableNames = new HashMap[String, String]
      tables.foreach{t => t.beforeSchemifier}
      tables.foreach{t => ensureTable(t, connection, actualTableNames)}
      tables.foreach{t => ensureColumns(t, connection, actualTableNames)}
      tables.foreach{t => ensureIndexes(t, connection, actualTableNames)}
      tables.foreach{t => ensureConstraints(t, connection, actualTableNames)}
      tables.foreach{t => t.afterSchemifier}
    }
  }
  
  private def calcDriver(name: String): DriverType = {
    name match {
      case "Apache Derby" => DerbyDriver
      case "MySQL" => MySqlDriver
    }
  }
  
  private def ensureTable(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]) {
    val md = connection.getMetaData
    val rs = md.getTables(null, null, null, null)
    var hasTable = false
    while (!hasTable && rs.next) {
      val tableName = rs.getString(3)
      hasTable = table.dbTableName.toLowerCase == tableName.toLowerCase
      if (hasTable) actualTableNames(table.dbTableName) = tableName
    }
    rs.close
    if (!hasTable) {
      val ct = "CREATE TABLE "+table.dbTableName+" ("+createColumns(table, connection).mkString("", " , ", "")+")";
      val st = connection.createStatement
      st.execute(ct)
      st.close
      
      table.mappedFields.filter{f => f.dbPrimaryKey_?}.foreach {
        pkField =>
        val ct = "ALTER TABLE "+table.dbTableName+" ADD CONSTRAINT "+table.dbTableName+"_PK PRIMARY KEY("+pkField.dbColumnName+")"
          val st = connection.createStatement
          st.execute(ct)
          st.close
      }
      
      val rs = md.getTables(null, null, null, null)
      var hasTable = false
      while (!hasTable && rs.next) {
        val tableName = rs.getString(3)
        hasTable = table.dbTableName.toLowerCase == tableName.toLowerCase
        if (hasTable) actualTableNames(table.dbTableName) = tableName
      }
      rs.close
    }
  }
  
  private def createColumns(table: BaseMetaMapper, connection: SuperConnection): Seq[String] = {
    table.mappedFields.flatMap(_.fieldCreatorString(connection.driverType))
  }

  private def ensureColumns(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]) {
    table.mappedFields.foreach {
      field =>
	var hasColumn = 0
      var cols: List[String] = Nil
      val totalColCnt = field.dbColumnCount
      val md = connection.getMetaData
      val rs = md.getColumns(null, null, actualTableNames(table.dbTableName), null)
      while (hasColumn < totalColCnt && rs.next) {
        val tableName = rs.getString(3).toLowerCase
        val columnName = rs.getString(4).toLowerCase
        
        if (tableName == table.dbTableName && field.dbColumnNames(field.name).contains(columnName)) {
          cols = columnName :: cols
          hasColumn = hasColumn + 1
        }
      }
      rs.close
      
      // FIXME deal with column types
      (field.dbColumnNames(field.name) diff cols).foreach {
        colName =>
          
          val ct = "ALTER TABLE "+table.dbTableName+" ADD COLUMN "+field.fieldCreatorString(connection.driverType, colName)
        val st = connection.createStatement
        st.execute(ct)
        st.close

        if (field.dbPrimaryKey_?) {
          val ct = "ALTER TABLE "+table.dbTableName+" ADD CONSTRAINT "+table.dbTableName+"_PK PRIMARY KEY("+field.dbColumnName+")"
            val st = connection.createStatement
            st.execute(ct)
            st.close
        }
      }
      
    }
    
  }

  private def ensureIndexes(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]) {
    table.mappedFields.filter{f => f.dbIndexed_?}.foreach {
      field =>
      val md = connection.getMetaData
      val rs = md.getIndexInfo(null, null, actualTableNames(table.dbTableName), false, false)
      var foundIt = false
      while (!foundIt && rs.next) {
        val tableName = rs.getString(3)
        val indexName = rs.getString(6)
        val columnName = rs.getString(9)

        foundIt = (tableName.toLowerCase == table.dbTableName.toLowerCase && field.dbColumnName.toLowerCase == columnName.toLowerCase)
      }
      rs.close
      
      if (!foundIt) {
        val ct = "CREATE INDEX "+(table.dbTableName+"_"+field.dbColumnName)+" ON "+table.dbTableName+" ( "+field.dbColumnName+" )"
        val st = connection.createStatement
        st.execute(ct)
        st.close
      }
    }
  }

  private def ensureConstraints(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]) {
    table.mappedFields.flatMap{f => f match {case f: BaseMappedField with BaseForeignKey => List(f); case _ => Nil}}.foreach {
      field =>
      
      val other = field.dbKeyToTable
      val otherTable = actualTableNames(other.dbTableName)
      val myTable = actualTableNames(table.dbTableName)
      
      val md = connection.getMetaData
      // val rs = md.getCrossReference(null, null,otherTable , null, null, myTable)
       val rs = md.getExportedKeys(null, null,myTable)
      //val rs = md.getCrossReference(null, null,myTable , null, null, otherTable)
      var foundIt = false
      while (!foundIt && rs.next) {
        val pkName = rs.getString(4)
        val fkName = rs.getString(8)
        foundIt = (field.dbColumnName.toLowerCase == fkName.toLowerCase && field.dbKeyToColumn.dbColumnName.toLowerCase == pkName.toLowerCase)
      }
      rs.close

      if (!foundIt) {
        val ct = "ALTER TABLE "+table.dbTableName+" ADD FOREIGN KEY ( "+field.dbColumnName+" ) REFERENCES "+other.dbTableName+" ( "+field.dbKeyToColumn.dbColumnName+" ) "
        val st = connection.createStatement
        st.execute(ct)
        st.close
      }
    }
  }
}

abstract class DriverType
object MySqlDriver extends DriverType
object DerbyDriver extends DriverType

