package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import com.hellolift.model._
 
/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    addToPackages("com.hellolift")
     
    Schemifier.schemify(true, User, Blog, Entry)
    LiftServlet.addTemplateBefore(User.templates) // LiftNote 5
    LiftServlet.setSiteMap(SiteMapBuilder.sitemap)
  }
}

object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Can[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm =  DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}

/*
 * Creating a SiteMap object.
 */
object SiteMapBuilder {
  def loggedIn_? = User.loggedIn_?
  def notLoggedIn_? = !loggedIn_?

  val sitemap = SiteMap(
    Menu(Loc("Home", "/", "Home")),
    Menu(Loc("Login", "/user_mgt/login", "Login", If(notLoggedIn_? _, "already logged in. Please logout first."))),
    Menu(Loc("Logout", "/user_mgt/logout", "Logout", If(loggedIn_? _, "You must be logged in to Logout."))),
    Menu(Loc("CreateUser", "/user_mgt/signup", "Create New User", If(notLoggedIn_? _, "Please logout first."))),
    Menu(Loc("LostPassword", "/user_mgt/lost_password", "Lost Password", If(notLoggedIn_? _, "Please logout first."))), // not logged in
    Menu(Loc("ResetPassword", "/user_mgt/reset_password", "Reset Password", Hidden, If(notLoggedIn_? _, "Please logout first."))), //not Logged in
    Menu(Loc("EditUser", "/user_mgt/edit", "Edit User", If(loggedIn_? _, "Please login first."))), // Logged in
    Menu(Loc("ChangePassword", "/user_mgt/change_password", "Change Password", If(loggedIn_? _, "Please login first."))), // Logged in
    Menu(Loc("ValidateUser", "/user_mgt/validate_user", "Validate User", Hidden, If(notLoggedIn_? _, "Please logout first."))), // Not Logged in
    Menu(Loc("CreateEntry", "/entry", "Create An Entry", If(loggedIn_? _, "Please login")))
    )
}
