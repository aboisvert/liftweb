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
  
  var connectionManager: Option[ConnectionManager] = None
  
  private def info : HashMap[String, (Connection, int)] = {
    threadStore.get.asInstanceOf[HashMap[String, (Connection, int)]] match {
      case null => {
      val tinfo = new HashMap[String, (Connection, int)];
      threadStore.set(tinfo)
      tinfo
    }
      case v => {v}
    }
  }
  
  private def whichName(name : String) = if (name == null || name.length == 0) "lift" else name
  
  private def newConnection(name : String) : Connection = 
    connectionManager.flatMap{cm => cm.newConnection(name)}.getOrElse {envContext.lookup(whichName(name)).asInstanceOf[DataSource].getConnection}
    
  
  private def releaseConnection(conn : Connection) : unit = conn.close
  
  private def getPairForName(name : String) : (Connection, int) =  {
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
  
  def statement[T](db : Connection)(f : (Statement) => T) : T =  {
    val st = db.createStatement
    try {
      f(st)
    } finally {
      st.close
    }
  }
  
  def exec[T](db : Connection, query : String)(f : (ResultSet) => T) : T = {
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
  
  def prepareStatement[T](statement : String)(f : (PreparedStatement) => T) : T = {
    use {
      conn =>
	val st = conn.prepareStatement(statement)
      try {
	f(st)
      } finally {
        st.close
      }
    }
  }
  
  def prepareStatement[T](statement : String,keys: int)(f : (PreparedStatement) => T) : T = {
    use {
      conn =>
        val st = conn.prepareStatement(statement, keys)
      try {
        f(st)
      } finally {
        st.close
      }
    }
  }
  
  def use[T](f : (Connection) => T) : T = {
    this.use("")(f)
  }
  def use[T](name : String)(f : (Connection) => T) : T = {
    try {
      f(getPairForName(name)._1)
    } finally {
      releaseConnectionNamed(name)
    }
  }
}
