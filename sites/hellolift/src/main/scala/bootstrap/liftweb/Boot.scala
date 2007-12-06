package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper._
import java.sql.{Connection, DriverManager}
import com.hellolift.model._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // add the connection manager if there's not already a JNDI connection defined
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor) 
    
    // add the com.hellolift package to the list packages
    // searched for Snippets, CometWidgets, etc.
    LiftServlet.addToPackages("com.hellolift")
     
    // Update the database schema to be in sync
    Schemifier.schemify(true, Log.infoF _, User, Entry)
    
    // Add a template handler to requests that come in for User related
    // function (e.g., log in, log out, etc.) are handled appropriately
    LiftServlet.addTemplateBefore(User.templates) // LiftNote 5
    
    // The locale is either calculated based on the incoming user or
    // based on the http request
    LiftServlet.localeCalculator = r => User.currentUser.map(_.locale.isAsLocale).openOr(LiftServlet.defaultLocaleCalculator(r))

    // Build SiteMap
    val entries = Menu(Loc("Home", "/", "Home")) :: 
    Menu(Loc("Request Details", "/request", "Request Details")) :: 
    User.sitemap ::: Entry.sitemap

    LiftServlet.setSiteMap(SiteMap(entries:_*))
    
    // lazily load the current user on a request-by-request basis
    S.addAround(User.requestLoans)    
  }
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm = DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}
