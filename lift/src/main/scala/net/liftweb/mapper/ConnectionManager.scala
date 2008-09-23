package net.liftweb.mapper


/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import _root_.java.sql.Connection
import _root_.net.liftweb.util._

/**
 * Vend JDBC connections
 */
trait ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection]
  def releaseConnection(conn: Connection)
}
