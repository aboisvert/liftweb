package bootstrap.liftweb

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.util.Helpers
import net.liftweb.http._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier}
import java.sql.{Connection, DriverManager}
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.immutable.TreeMap
import com.skittr.model._
import com.skittr.actor._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager("", DBVendor)
    addToPackages("com.skittr")
     
    // make sure the database is up to date
    Schemifier.schemify(User, Friend, MsgStore)
    
    if ((System.getProperty("create_users") != null) && User.count < User.createdCount) User.createTestUsers
    
    // map certain urls to the right place
    val rewriter: Servlet.rewritePf = {
    case (_, ParsePath("user" :: user :: _, _,_), _, _) => ("/user", ParsePath("user" :: Nil, true, false), TreeMap("user" -> user))
    case (_, ParsePath("friend" :: user :: _, _,_), _, _) => ("/friend", ParsePath("friend" :: Nil, true, false), TreeMap("user" -> user))
    case (_, ParsePath("unfriend" :: user :: _, _,_), _, _) => ("/unfriend", ParsePath("unfriend" :: Nil, true, false), TreeMap("user" -> user))
  }
  
  Servlet.addRewriteBefore(rewriter)
  
  // load up the list of user actors
  UserList.create
  }
}

/**
  * A singleton that vends a database connection to a Derby database
  */
object DBVendor extends ConnectionManager {
  def newConnection(name: String): Option[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm =  DriverManager.getConnection("jdbc:derby:skittr;create=true")

      Some(dm)
    } catch {
      case e : Exception => e.printStackTrace; None
    }
  }
}
