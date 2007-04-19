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
import net.liftweb.example.controller.WebServices
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import scala.collection.immutable.TreeMap
import net.liftweb.example.model._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    DB.defineConnectionManager("", DBVendor)
    addToPackages("net.liftweb.example")
     
    Schemifier.schemify(User, WikiEntry)
    
    val dispatcher: PartialFunction[(RequestState, List[String], (String) => java.io.InputStream),(HttpServletRequest) => Option[Any]] = 
      {
    case (r, "webservices" :: c :: _, _) => { 
          (req: HttpServletRequest) => {
          val rc = new WebServices(r, req)
          val invoker = createInvoker(c, rc)
          invoker match {
            case None => None
            case Some(f) => f()
          }
          }
        }
    }
    Servlet.addDispatchBefore(dispatcher)
    
    val rewriter: Servlet.rewritePf = {
      case (_, path @ "wiki" :: page :: _, _, _) => ("/wiki", "wiki" :: Nil, 
          TreeMap("wiki_page" -> page :: path.drop(2).zipWithIndex.map(p => ("param"+(p._2 + 1)) -> p._1) :_*))
    }
    
    Servlet.addRewriteBefore(rewriter)
  }
}

object DBVendor extends ConnectionManager {
  def newConnection(name: String): Option[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm =  DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Some(dm)
    } catch {
      case e : Exception => e.printStackTrace; None
    }
  }
}