package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.sql.{Connection, ResultSet, Statement, PreparedStatement, Types, ResultSetMetaData}
import javax.sql.{ DataSource}
import javax.naming.{Context, InitialContext}
import scala.collection.mutable._
import net.liftweb.util._
// import net.liftweb.util.Lazy._

object DB {
  private val threadStore = new ThreadLocal
  private val envContext = FatLazy((new InitialContext).lookup("java:/comp/env").asInstanceOf[Context])
  val logger = Log.logger(DB.getClass)
  
  var queryTimeout: Can[Int] = Empty
  
  private var logFuncs: List[(String, Long) => Any] = Nil
  
  def addLogFunc(f: (String, Long) => Any): List[(String, Long) => Any] = {
    logFuncs = logFuncs ::: List(f)
    logFuncs
  }
  
  /**
    * can we get a JDBC connection from JNDI?
    */
  def jndiJdbcConnAvailable_? : Boolean = {
    val touchedEnv = envContext.calculated_?
    
    val ret = try {
      (envContext.get.lookup(DefaultConnectionIdentifier.jndiName).asInstanceOf[DataSource].getConnection) != null
    } catch {
      case e => false
    }
        
    if (!touchedEnv) envContext.reset
    ret
  }
  
  // var connectionManager: Can[ConnectionManager] = Empty
  private val connectionManagers = new HashMap[ConnectionIdentifier, ConnectionManager];
  
  def defineConnectionManager(name: ConnectionIdentifier, mgr: ConnectionManager) {
    connectionManagers(name) = mgr
  }
  
  case class ConnectionHolder(conn: SuperConnection, cnt: Int, postCommit: List[() => Unit]) 
  
  private def info : HashMap[ConnectionIdentifier, ConnectionHolder] = {
    threadStore.get match {
      case null =>
	val tinfo = new HashMap[ConnectionIdentifier, ConnectionHolder];
	threadStore.set(tinfo)
	tinfo

      case v: HashMap[ConnectionIdentifier, ConnectionHolder] => v
    }
  }
  
  // remove thread-local association
  def clearThread: Unit = { threadStore.remove }  

  private def newConnection(name : ConnectionIdentifier) : SuperConnection = {
    val ret = (Can(connectionManagers.get(name)).flatMap(cm => cm.newConnection(name).map(c => new SuperConnection(c, () => cm.releaseConnection(c))))) openOr {
      Helpers.tryo {
      val conn = envContext.get.lookup(name.jndiName).asInstanceOf[DataSource].getConnection
      new SuperConnection(conn, () => conn.close)
      } openOr {throw new NullPointerException("Looking for Connection Identifier "+name+" but failed to find either a JNDI data source "+
        "with the name "+name.jndiName+" or a lift connection manager with the correct name")}
    }
    ret.setAutoCommit(false)
    ret
  }
  
  
  
  private def releaseConnection(conn : SuperConnection) : unit = conn.close
  
  private def getConnection(name : ConnectionIdentifier): SuperConnection =  {
    var ret = info.get(name) match {
      case None => ConnectionHolder(newConnection(name), 1, Nil)
      case Some(ConnectionHolder(conn, cnt, post)) => ConnectionHolder(conn, cnt + 1, post)
    }
    info(name) = ret
    ret.conn
  }
  
  private def releaseConnectionNamed(name: ConnectionIdentifier) {
    info.get(name) match {
      case Some(ConnectionHolder(c, 1, post)) => c.commit; c.releaseFunc(); info -= name; post.reverse.foreach(_()); logger.trace("Released connection "+name)
      case Some(ConnectionHolder(c, n, post)) => info(name) = ConnectionHolder(c, n - 1, post)
      case _ =>
    }
  }
  
  /**
    *  Append a function to be invoked after the commit has taken place for the given connection identifier
    */
  def appendPostFunc(name: ConnectionIdentifier, func: () => Unit) {
    info.get(name) match {
      case Some(ConnectionHolder(c, n, post)) => info(name) = ConnectionHolder(c, n, func :: post)
      case _ =>
    }
  }
  
  private def runLogger(query: String, time: Long) {
    logFuncs.foreach(_(query, time))
  }
  
  def statement[T](db : SuperConnection)(f : (Statement) => T) : T =  {
    Helpers.calcTime {
    val st = db.createStatement
    queryTimeout.foreach(to => st.setQueryTimeout(to))
    try {
      (st.toString, f(st))
    } finally {
      st.close
    }
    } match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def exec[T](db : SuperConnection, query : String)(f : (ResultSet) => T) : T = {
    Helpers.calcTime(
    statement(db) {st => 
      f(st.executeQuery(query))
      }) match {case (time, res) => runLogger(query, time); res}
  }
  
  
  
  def runQuery(query: String): (List[String], List[List[String]]) = {
    import java.sql.Types._
    
    def asString(pos: Int, rs: ResultSet, md: ResultSetMetaData): String = md.getColumnType(pos) match {
      
      case ARRAY | BINARY | BLOB | DATALINK | DISTINCT | JAVA_OBJECT | LONGVARBINARY | NULL | OTHER | REF | STRUCT | VARBINARY  => rs.getObject(pos) match {
        case null => null
        case s => s.toString
      }
      case BIGINT |  INTEGER | DECIMAL | NUMERIC | SMALLINT | TINYINT => rs.getLong(pos).toString
      case BIT | BOOLEAN => rs.getBoolean(pos).toString
      
      case VARCHAR | CHAR | CLOB | LONGVARCHAR => rs.getString(pos)

      case DATE | TIME | TIMESTAMP => rs.getTimestamp(pos).toString
      
      case DOUBLE | FLOAT | REAL  => rs.getDouble(pos).toString
    }
    
    use(DefaultConnectionIdentifier)(conn => exec(conn, query) {
      rs =>
      val md = rs.getMetaData
      val cnt = md.getColumnCount
      val cntList = (1 to cnt).toList
      val colNames = cntList.map(i => md.getColumnName(i))
      
      val lb = new ListBuffer[List[String]]()
      
      while(rs.next) {
        lb += cntList.map(i => asString(i, rs, md))
      }
      
      (colNames, lb.toList)
    })
  }
  
  def rollback(name: ConnectionIdentifier) = use(name)(conn => conn.rollback)
  
  def exec[T](statement : PreparedStatement)(f : (ResultSet) => T) : T = {
    queryTimeout.foreach(to => statement.setQueryTimeout(to))
    Helpers.calcTime{
    val rs = statement.executeQuery
    try {
      (statement.toString, f(rs))
    } finally {
      rs.close
    }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def prepareStatement[T](statement : String, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
    Helpers.calcTime {
    val st = conn.prepareStatement(statement)
    queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
	(st.toString, f(st))
      } finally {
        st.close
      }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def prepareStatement[T](statement : String, keys: int, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
    Helpers.calcTime{
        val st = conn.prepareStatement(statement, keys)
        queryTimeout.foreach(to => st.setQueryTimeout(to))
      try {
        (st.toString, f(st))
      } finally {
        st.close
      }} match {case (time, (query, res)) => runLogger(query, time); res}
  }
  
  def use[T](name : ConnectionIdentifier)(f : (SuperConnection) => T) : T = {
    val conn = getConnection(name)
    try {
      f(conn)
    } finally {
      releaseConnectionNamed(name)
    }
  }
}


abstract class DriverType
object MySqlDriver extends DriverType
object DerbyDriver extends DriverType


class SuperConnection(val connection: Connection,val releaseFunc: () => Any) {
  lazy val brokenLimit_? = (connection.getMetaData.getDatabaseProductName ==  "Apache Derby")
  def createTablePostpend: String = driverType match {
    case DerbyDriver => ""
    case MySqlDriver => " ENGINE = InnoDB "
  }
  
  def supportsForeignKeys_? : Boolean = driverType match {
    case DerbyDriver => false
    case MySqlDriver => false
  }
  
 lazy val driverType = (calcDriver(connection.getMetaData.getDatabaseProductName))
    
  def calcDriver(name: String): DriverType = {
    name match {
      case "Apache Derby" => DerbyDriver
      case "MySQL" => MySqlDriver
    }
  }  
}

object SuperConnection {
  implicit def superToConn(in: SuperConnection): Connection = in.connection
}

trait ConnectionIdentifier {
  def jndiName: String
}

case object DefaultConnectionIdentifier extends ConnectionIdentifier {
  var jndiName = "lift"
}
