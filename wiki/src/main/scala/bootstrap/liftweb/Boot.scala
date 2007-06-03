package bootstrap.liftweb

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.util.Helpers
import net.liftweb.http._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.immutable.TreeMap
import com.liftweb.wiki.model._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    addToPackages("com.liftweb.wiki")
    
    Schemifier.schemify(User, Revision, Paragraph, Page)

    // TODO make it works
    val rewriter: Servlet.rewritePf = {
      case RewriteRequest(_, ParsePath("page" :: page :: _, _,_), _, _) => 
        RewriteResponse("/index", ParsePath("page" :: Nil, true, false), TreeMap("page" -> page))
    }
  
    Servlet.addRewriteBefore(rewriter)
  }
}

/**
  * A singleton that vends a database connection to a Derby database
  */
object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Option[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm =  DriverManager.getConnection("jdbc:derby:wiki;create=true")

      Some(dm)
    } catch {
      case e : Exception => e.printStackTrace; None
    }
  }
  def releaseConnection(conn: Connection) { conn.close }
}
