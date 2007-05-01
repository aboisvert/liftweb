package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.sql.{Connection, ResultSet, Statement, PreparedStatement}
import javax.sql.{ DataSource}
import javax.naming.{Context, InitialContext}
import scala.collection.mutable._
import net.liftweb.util._
import net.liftweb.util.Lazy._

object DB {
  private val threadStore = new ThreadLocal
  private val envContext = Lazy{(new InitialContext).lookup("java:/comp/env").asInstanceOf[Context]}
  
  /**
    * can we get a JDBC connection from JNDI?
    */
  def jndiJdbcConnAvailable_? : boolean = {
    val touchedEnv = envContext.calculated_?
    
    val ret = try {
      (envContext.lookup(whichName("lift")).asInstanceOf[DataSource].getConnection) != null
    } catch {
      case e => false
    }
        
    if (!touchedEnv) envContext.reset
    ret
  }
  
  // var connectionManager: Option[ConnectionManager] = None
  private val connectionManagers = new HashMap[String, ConnectionManager];
  
  def defineConnectionManager(name: String, mgr: ConnectionManager) {
    connectionManagers(name) = mgr
  }
  
  private def info : HashMap[String, (SuperConnection, int)] = {
    threadStore.get.asInstanceOf[HashMap[String, (SuperConnection, int)]] match {
      case null => {
	val tinfo = new HashMap[String, (SuperConnection, int)];
	threadStore.set(tinfo)
	tinfo
      }
      case v => {v}
    }
  }
  
  private def whichName(name : String) = if (name == null || name.length == 0) "lift" else name
  
  private def newConnection(name : String) : SuperConnection = 
    new SuperConnection(connectionManagers.get(name).flatMap{cm => cm.newConnection(name)}.getOrElse {envContext.lookup(whichName(name)).asInstanceOf[DataSource].getConnection})
  
  
  private def releaseConnection(conn : SuperConnection) : unit = conn.close
  
  private def getPairForName(name : String) : (SuperConnection, int) =  {
    var ret = info.get(name) match {
      case None => (newConnection(name), 1)
	case c => (c.get._1, c.get._2 + 1)
    }
    info += name -> ret
    ret
  }
  
  def releaseConnectionNamed(name : String) {
    info.get(name) match {
      case None => {}
      case Some(c)  => {c match {
	case (c , 1) => releaseConnection(c); info -= name
	case (c, cnt) => info(name) = (c,cnt - 1)
      }
		      }
    }
  }
  
  def statement[T](db : SuperConnection)(f : (Statement) => T) : T =  {
    val st = db.createStatement
    try {
      f(st)
    } finally {
      st.close
    }
  }
  
  def exec[T](db : SuperConnection, query : String)(f : (ResultSet) => T) : T = {
    statement(db) {st => 
      f(st.executeQuery(query))
		 }
  }
  
  def exec[T](statement : PreparedStatement)(f : (ResultSet) => T) : T = {
    val rs = statement.executeQuery
    try {
      f(rs)
    } finally {
      rs.close
    }
  }
  
  def prepareStatement[T](statement : String, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
    
	val st = conn.prepareStatement(statement)
      try {
	f(st)
      } finally {
        st.close
      }
    
  }
  
  def prepareStatement[T](statement : String,keys: int, conn: SuperConnection)(f : (PreparedStatement) => T) : T = {
        val st = conn.prepareStatement(statement, keys)
      try {
        f(st)
      } finally {
        st.close
      }
  }
  
  def use[T](f : (SuperConnection) => T) : T = {
    this.use("")(f)
  }
  def use[T](name : String)(f : (SuperConnection) => T) : T = {
    try {
      f(getPairForName(name)._1)
    } finally {
      releaseConnectionNamed(name)
    }
  }
}

class SuperConnection(val connection: Connection) {
  val brokenLimit_? : Lazy[boolean] = Lazy {
    connection.getMetaData.getDatabaseProductName ==  "Apache Derby"
  }
}

object SuperConnection {
  implicit def superToConn(in: SuperConnection): Connection = in.connection
}
