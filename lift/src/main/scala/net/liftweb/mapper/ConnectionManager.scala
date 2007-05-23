package net.liftweb.mapper


/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.sql.Connection

/**
 * Vend JDBC connections
 */ 
trait ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Option[Connection]
  // def releaseConnection(conn: Connection)  
}
