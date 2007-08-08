package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.mapper._
import net.liftweb.http._
import scala.xml.{NodeSeq, Node, Group}
import scala.xml.transform._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.util.Mailer._
import S._

trait ProtoUser[T <: ProtoUser[T]] extends KeyedMapper[Long, T] {
  // the primary key for the database
  object id extends MappedLongIndex[T](this)
  
  // First Name
  object firstName extends MappedString[T](this, 32)

  // Last Name
  object lastName extends MappedString[T](this, 32)

  // Email
  object email extends MappedEmail[T](this, 48) {
    override def dbIndexed_? = true
    override def validations = valUnique("The email address must be unique") _ :: super.validations
  }

  // Password
  object password extends MappedPassword[T](this)
}

trait SuperProtoUser[T <: SuperProtoUser[T]] extends ProtoUser[T] {
  object uniqueId extends MappedUniqueId[T](this, 32) {
    override def dbIndexed_? = true
  }
  
  object validated extends MappedBoolean[T](this) {
    override def defaultValue: Boolean = false
  }
  
  def signupFields = firstName :: lastName :: email :: password :: Nil
  
  /**
    * If the 
    */
  def screenWrap: Option[Node] = None
  
  val BasePath = "user_mgt"
  val SignUp = "sign_up"
  val Login = "login"
  val LostPassword = "lost_password"
  val PasswordReset = "reset_password"
  val ChangePassword = "change_password"
  val Logout = "logout"
  val Edit = "edit"
  val ValidateUser = "validate_user"
  
  val HomePage = "/"
  
    case class MenuItem(name: String, endOfPath: String, loggedIn: Boolean) {
    def path = "/"+BasePath+"/"+endOfPath
    def display = name match {
      case null | "" => false
      case _ => true
    }
  }
  
  def userMenu: List[Node] = {
    val li = loggedIn_?
    ItemList.filter(i => i.display && i.loggedIn == li).map(i => <a href={i.path}>{i.name}</a>)
  }
  
  val ItemList: List[MenuItem] = MenuItem("Sign Up", SignUp, false) :: MenuItem("Log In", Login, false) :: 
  MenuItem("Lost Password", LostPassword, false) ::
  MenuItem("", PasswordReset, false) :: MenuItem("Change Password", ChangePassword, true) :: 
  MenuItem("Log Out", Logout, true) :: MenuItem("Edit Profile", Edit, true) :: MenuItem("", ValidateUser, false) :: Nil
    
  def templates: Servlet.TemplatePf = {
  case RequestMatcher(_, ParsePath(BasePath :: (w @ SignUp) :: _, _, _)) if testLoggedIn(w) => () => signup
  case RequestMatcher(_, ParsePath(BasePath :: (w @ Login) :: _, _, _)) if testLoggedIn(w) => () => login
  case RequestMatcher(_, ParsePath(BasePath :: (w @ LostPassword) :: _, _, _)) if testLoggedIn(w) => () => lostPassword
  case RequestMatcher(_, ParsePath(BasePath :: (w @ PasswordReset) :: id :: _, _, _)) if testLoggedIn(w) => () => passwordReset(id)
  case RequestMatcher(_, ParsePath(BasePath :: (w @ ChangePassword) :: _, _, _)) if testLoggedIn(w) => () => changePassword
  case RequestMatcher(_, ParsePath(BasePath :: (w @ Logout) :: _, _, _)) if testLoggedIn(w) => () => logout
  case RequestMatcher(_, ParsePath(BasePath :: (w @ Edit) :: _, _, _)) if testLoggedIn(w) => () => edit
  case RequestMatcher(_, ParsePath(BasePath :: (w @ ValidateUser) :: id :: _, _, _)) if testLoggedIn(w) => () => validateUser(id)
  }
  
  val LoggedInUserIdentifier = "$_proto_user_current_user"
  
  def loggedIn_? : Boolean = currentUserId.isDefined
      
  def logUserIn(who: T) {S.set(LoggedInUserIdentifier, who.id.toString)}
  def logoutCurrentUser {S.unset(LoggedInUserIdentifier)}
  
  def currentUserId: Option[String] = S.get(LoggedInUserIdentifier)
  
  def currentUser: Option[T] = currentUserId.flatMap(id => getSingleton.find(id))
  
  def signupXhtml(user: T) = <form method="POST" action={S.action}><table><tr><td colspan="2">Sign Up</td></tr>
  {localForm(user, false)}
  <tr><td>&nbsp;</td><td><user:submit/></td></tr>
  </table></form>
  
  def signup = {
    val theUser: T = getSingleton.create
    val theName = BasePath + SignUp

    def testSignup(ignore: String) {
      theUser.validate match {
        case Nil => S.removeSessionTemplater(theName)
        val user = theUser
        user.uniqueId.reset()
        theUser.save
        val resetLink = S.hostAndPath+"/"+BasePath+"/"+ValidateUser+"/"+user.uniqueId
        val email: String = user.email


        val msgXml =
          <html>
        <head>
        <title>Sign Up Confirmation</title>
        </head>
        <body>
        <p>Dear {user.firstName},
        <br/>
        <br/>
        Click on this link to complete signup
        <br/><a href={resetLink}>{resetLink}</a>
        <br/>
        <br/>
        Thanks
        </p>
        </body>
        </html>

        Mailer.sendMail(From("noreply@"+S.hostName),Subject("Sign up confirmation"),  To(user.email), msgXml)                
        S.notice("You have signed up.  A validation email message will be sent to you.")
        S.redirectTo(HomePage)
        
        case xs => S.error(xs)
      }
    }
    
    def innerSignup = bind("user", signupXhtml(theUser), "submit" -> submit("Sign Up", testSignup))

    S.addSessionTemplater(theName, {case RequestMatcher(_, ParsePath(BasePath :: (w @ SignUp) :: _, _, _)) if testLoggedIn(w) => 
    () => innerSignup})
    innerSignup
  }
  
  def testLoggedIn(page: String): Boolean =
    ItemList.filter(_.endOfPath == page) match {
      case x :: xs if x.loggedIn == loggedIn_? => true
      case _ => false
    }

  
  def validateUser(id: String) = getSingleton.find(By(uniqueId, id)) match {
  case Some(user) if !user.validated => 
  user.validated(true).uniqueId.reset().save
  S.notice("You have been validated... please log into your account")
  S.redirectTo("/"+BasePath+"/"+Login)  
  
  case _ => S.error("Validation link invalid"); S.redirectTo(HomePage)
  }
  
  def loginXhtml = <form method="POST" action={S.action}><table><tr><td colspan="2">Log In</td></tr>
  <tr><td>EMail Address</td><td><user:email /></td></tr>
  <tr><td>Password</td><td><user:password /></td></tr>
  <tr><td><a href={"/"+BasePath+"/"+LostPassword}>Recover Password</a></td><td><user:submit /></td></tr></table>
  </form>
  
  def login = {
    var username = ""
    var pwd = ""

    def testLogin(ignore: String) {
      getSingleton.find(By(email, username)) match {
        case Some(user) if user.validated && user.password.match_?(pwd) => logUserIn(user); S.notice("Logged In"); S.redirectTo(HomePage)
        case _ => S.error("Invalid Username/Password")
      }
    }
      
    bind("user", loginXhtml, "email" -> text("", username = _), "password" -> S.password("", pwd = _), "submit" -> submit("Log In", testLogin))
  }
  
  def lostPasswordXhtml = <form method="POST" action={S.action}>
  <table><tr><td colspan="2">Enter your email address and we'll email you a link to reset your password</td></tr>
  <tr><td>Email address</td><td><user:email /></td></tr>
  <tr><td>&nbsp;</td><td><user:submit /></td></tr>
  </table>
  </form>
  
  def lostPassword = {
    var email = ""
    
    def sendPasswordReset(ignore: String) {
      getSingleton.find(By(this.email, email), By(this.validated, true)) match {
        case Some(user) =>
        user.uniqueId.reset()
        val resetLink = S.hostAndPath+"/"+BasePath+"/"+PasswordReset+"/"+user.uniqueId
        val email: String = user.email


        val msgXml =
          <html>
        <head>
        <title>Reset Password Confirmation</title>
        </head>
        <body>
        <p>Dear {user.firstName},
        <br/>
        <br/>
        Click on this link to reset your password
        <br/><a href={resetLink}>{resetLink}</a>
        <br/>
        <br/>
        Thanks
        </p>
        </body>
        </html>

        Mailer.sendMail(From("noreply@"+S.hostName),Subject("Reset Password Request"),  To(user.email), msgXml)        
        S.notice("Password Reset Email sent") 
        S.redirectTo(HomePage)
        case None => S.error("Email address not found")
      }
    }
      
    bind("user", lostPasswordXhtml, "email" -> text("", email = _), "submit" -> submit("Send It", sendPasswordReset))
  }
  
  def passwordResetXhtml = <form method="POST" action={S.action}>
  <table><tr><td colspan="2">Reset your password</td></tr>
  <tr><td>Enter your new password</td><td><user:pwd/></td></tr>
  <tr><td>Enter your new password (repeat)</td><td><user:pwd/></td></tr>
  <tr><td>&nbsp;</td><td><user:submit/></td></tr>
  </table>
  </form>
  
  def passwordReset(id: String) = getSingleton.find(By(uniqueId, id)) match {
    case None => S.error("Password reset link invalid"); S.redirectTo(HomePage)
    case Some(user) => 
    def finishSet(ignore: String) {
      user.validate match {
        case Nil => S.notice("Password Changed"); user.save; logUserIn(user); S.redirectTo(HomePage)
        case xs => S.error(xs)
      }
    }
    user.uniqueId.reset().save
    
    bind("user", passwordResetXhtml, "pwd" -> password_*("",(p: List[String]) => user.password.setList(p)),
        "submit" -> submit("Set Password", finishSet))
  }
  
  def changePasswordXhtml = <form method="POST" action={S.action}>
  <table><tr><td colspan="2">Change Password</td></tr>
  <tr><td>Old Password</td><td><user:old_pwd /></td></tr>
  <tr><td>New Password</td><td><user:new_pwd /></td></tr>
  <tr><td>New Password (repeat)</td><td><user:new_pwd /></td></tr>
  <tr><td>&nbsp;</td><td><user:submit /></td></tr>
  </table>
  </form>
  
  def changePassword = {
    val user = currentUser.get // we can do this because the logged in test has happened
    var oldPassword = ""
    var newPassword: List[String] = Nil
    
    def testAndSet(ignore: String) {
      if (!user.password.match_?(oldPassword)) S.error("Wrong old password")
      else {
        user.password ::= newPassword
        user.validate match {
          case Nil => user.save; S.notice("Password Changed"); S.redirectTo(HomePage)
          case xs => S.error(xs)
        }
      }
    }
      
    bind("user", changePasswordXhtml, "old_pwd" -> S.password("", oldPassword = _), 
        "new_pwd" -> password_*("", L2FuncHolder(newPassword = _)), "submit" -> testAndSet _)
  }
  
  def editXhtml(user: T) = <form method="POST" action={S.action}>
  <table><tr><td colspan="2">Edit</td></tr>
  {localForm(user, true)}
  <tr><td>&nbsp;</td><td><user:submit/></td></tr>
  </table>
  </form>
  
  def edit = {
    val theUser: T = currentUser.get // we know we're logged in
    val theName = BasePath + Edit

    def testEdit(ignore: String) {
      theUser.validate match {
        case Nil => S.removeSessionTemplater(theName)
        theUser.save
        S.notice("You have updated your profile")
        S.redirectTo(HomePage)
        
        case xs => S.error(xs)
      }
    }
    
    def innerEdit = bind("user", editXhtml(theUser), "submit" -> submit("Edit", testEdit))

    S.addSessionTemplater(theName, {case RequestMatcher(_, ParsePath(BasePath :: (w @ Edit) :: _, _, _)) if testLoggedIn(w) => 
    () => innerEdit})
    innerEdit
  }
  
  def logout = {
    logoutCurrentUser
    S.redirectTo(HomePage)
  }
  
  private def localForm(user: T, ignorePassword: Boolean): NodeSeq = signupFields.map(fi => getSingleton.getActualField(user, fi)).
    filter(f => !ignorePassword || (f match {
      case f: MappedPassword[T] => false
      case _ => true
    })).
    map(f => <tr><td>{f.displayName}</td><td>{f.toForm}</td></tr>)
  
  protected implicit def nodeSeqToOption(in: NodeSeq): Option[NodeSeq] = 
    screenWrap.map{theDoc => 
    val rw = new RewriteRule {
      override def transform(n: Node) = n match {
        case <here /> => in
        case _ => n
      }
    }
    (new RuleTransformer(rw))(theDoc)
    } orElse Some(in)
}
