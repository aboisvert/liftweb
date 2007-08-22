package net.liftweb.mapper

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.sql.{Connection, ResultSet}
import scala.collection.mutable.HashMap
import net.liftweb.util.{Log, Helpers}
import Helpers._

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
   /*
  case class SuperConnection(connection: Connection) {
    val driverType = calcDriver(connection.getMetaData.getDatabaseProductName)
  }
   */
  implicit def superToRegConnection(sc: SuperConnection): Connection = sc.connection

  def schemify(stables: BaseMetaMapper*): unit = schemify(DefaultConnectionIdentifier, stables :_*)

  def schemify(dbId: ConnectionIdentifier, stables: BaseMetaMapper*) {
    val tables = stables.toList
    DB.use(dbId) {
      con =>
	val connection = con // SuperConnection(con)
      val driver = con.calcDriver(connection.getMetaData.getDatabaseProductName)
      val actualTableNames = new HashMap[String, String]
      tables.foreach{t => t.beforeSchemifier}
        val toRun = tables.flatMap(t => ensureTable(t, connection, actualTableNames) ) :::
      tables.flatMap{t => ensureColumns(t, connection, actualTableNames)} :::
      tables.flatMap{t => ensureIndexes(t, connection, actualTableNames)} :::
      tables.flatMap{t => ensureConstraints(t, connection, actualTableNames)}
      
      tables.foreach{t => t.afterSchemifier}
      toRun.foreach{f => f()}
    }
  }
  
  def destroyTables_!!(stables: BaseMetaMapper*): Unit = destroyTables_!!(DefaultConnectionIdentifier, stables :_*)

  def destroyTables_!!(dbId: ConnectionIdentifier, stables: BaseMetaMapper*): Unit = destroyTables_!!(dbId, 0, stables.toList)

  def destroyTables_!!(dbId: ConnectionIdentifier,cnt: Int, stables: List[BaseMetaMapper]) {
    val th = new HashMap[String, String]()
    (DB.use(dbId) {
      conn =>
      val sConn = conn // SuperConnection(conn)
      val tables = stables.toList.filter(t => hasTable_?(t, sConn, th))
      
      tables.foreach{
        table =>
        try {
          val ct = "DROP TABLE "+table.dbTableName
          val st = conn.createStatement
          st.execute(ct)
          Log.trace(ct)
          st.close
        } catch {
          case e: Exception => // dispose... probably just an SQL Exception
        }
      }
      
      tables
    }) match {
      case t if t.length > 0 && cnt < 1000 => destroyTables_!!(dbId, cnt + 1, t)
      case _ =>
    }
  }
  
  private def hasTable_? (table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): Boolean = {
    val md = connection.getMetaData
    val rs = md.getTables(null, null, null, null)
    
    def hasTable: Boolean = {
      if (!rs.next) false
      else rs.getString(3) match {
        case s if s.toLowerCase == table.dbTableName.toLowerCase => actualTableNames(table.dbTableName) = s; true
        case _ => hasTable
      }
    }

    hasTable
  }
  
  private def ensureTable(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): List[() => Any] = {
    val hasTable = hasTable_?(table, connection, actualTableNames)
    if (!hasTable) {
      val ct = "CREATE TABLE "+table.dbTableName+" ("+createColumns(table, connection).mkString(" , ")+") "+connection.createTablePostpend
      val st = connection.createStatement
      Log.trace(ct)
      st.execute(ct)
      st.close
      table.mappedFields.filter{f => f.dbPrimaryKey_?}.foreach {
        pkField =>
        val ct = "ALTER TABLE "+table.dbTableName+" ADD CONSTRAINT "+table.dbTableName+"_PK PRIMARY KEY("+pkField.dbColumnName+")"
          val st = connection.createStatement
          st.execute(ct)
          st.close
      }
      
      hasTable_?(table, connection, actualTableNames)
      table.dbAddTable.toList
    } else Nil
  }
  
  private def createColumns(table: BaseMetaMapper, connection: SuperConnection): Seq[String] = {
    table.mappedFields.flatMap(_.fieldCreatorString(connection.driverType))
  }

  private def ensureColumns(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): List[() => unit] = {
    table.mappedFields.toList.flatMap {
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
      
      field.dbAddedColumn.toList
      
    }
    
  }

  private def ensureIndexes(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): List[() => Unit] = {
    
    // val byColumn = new HashMap[String, List[(String, String, Int)]]()
    val byName = new HashMap[String, List[String]]()
                               
    val md = connection.getMetaData
    val rs = md.getIndexInfo(null, null, actualTableNames(table.dbTableName), false, false)
    def quad(rs: ResultSet): List[(String, String, Int)] = {
      if (!rs.next) Nil else {
      if (rs.getString(3).toLowerCase == table.dbTableName.toLowerCase)
        (rs.getString(6).toLowerCase, rs.getString(9).toLowerCase, rs.getInt(8)) :: quad(rs)
      else Nil
      }
    }
    
    val q = quad(rs)
    // q.foreach{case (name, col, pos) => byColumn.get(col) match {case Some(li) => byColumn(col) = (name, col, pos) :: li case _ => byColumn(col) = List((name, col, pos))}}
    q.foreach{case (name, col, pos) => byName.get(name) match {case Some(li) => byName(name) = col :: li case _ => byName(name) = List(col)}}
    val indexedFields: List[List[String]] = byName.map{case (name, value) => value.sort(_ < _)}.toList
    rs.close
    
    val single = table.mappedFields.filter{f => f.dbIndexed_?}.toList.flatMap {
      field =>
      if (!indexedFields.contains(List(field.dbColumnName.toLowerCase))) {
        val ct = "CREATE INDEX "+(table.dbTableName+"_"+field.dbColumnName)+" ON "+table.dbTableName+" ( "+field.dbColumnName+" )"
          Log.trace(ct)
          val st = connection.createStatement
          st.execute(ct)
          st.close
          field.dbAddedIndex.toList
      } else Nil
    }
    
    table.dbIndexes.foreach {
      index =>
      val columns = index.columns.toList
      val fn = columns.map(_.field.dbColumnName.toLowerCase).sort(_ < _)
      if (!indexedFields.contains(fn)) {
          val ct = "CREATE INDEX "+(table.dbTableName+"_"+columns.map(_.field.dbColumnName).mkString("_"))+" ON "+table.dbTableName+" ( "+columns.map(_.indexDesc).comma+" )"
          Log.trace(ct)
          val st = connection.createStatement
          st.execute(ct)
          st.close
      }
    }
    
    single
  }

  private def ensureConstraints(table: BaseMetaMapper, connection: SuperConnection, actualTableNames: HashMap[String, String]): List[() => Unit] = {
    if (connection.supportsForeignKeys_?) {
    table.mappedFields.flatMap{f => f match {case f: BaseMappedField with BaseForeignKey => List(f); case _ => Nil}}.toList.flatMap {
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
          Log.trace(ct)
        val st = connection.createStatement
        st.execute(ct)
        st.close
        field.dbAddedForeignKey.toList
      } else Nil
    }
    } else Nil
  }
}

