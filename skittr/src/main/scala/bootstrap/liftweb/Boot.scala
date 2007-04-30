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
    // DB.defineConnectionManager("", DBVendor)
    addToPackages("com.skittr")
     
    Schemifier.schemify(User, Friend, MsgStore)
    
    val rewriter: Servlet.rewritePf = {
    case (_, path @ "user" :: user :: _, _, _) => ("/user", "user" :: Nil, 
        TreeMap("user" -> user :: path.drop(2).zipWithIndex.map(p => ("param"+(p._2 + 1)) -> p._1) :_*))
  }
  
  Servlet.addRewriteBefore(rewriter)
  UserList.create
  }
}

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
