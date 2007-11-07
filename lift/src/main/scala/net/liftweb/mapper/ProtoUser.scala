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
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.util.Mailer._
import S._

trait ProtoUser[T <: ProtoUser[T]] extends KeyedMapper[Long, T] { self: T =>
  // the primary key for the database
  object id extends MappedLongIndex(this)
  
  // First Name
  object firstName extends MappedString(this, 32)

  // Last Name
  object lastName extends MappedString(this, 32)

  // Email
  object email extends MappedEmail(this, 48) {
    override def dbIndexed_? = true
    override def validations = valUnique("The email address must be unique") _ :: super.validations
  }

  // Password
  object password extends MappedPassword[T](this)
  
  
  object superUser extends MappedBoolean(this) {
    override def defaultValue = false
  }
  
  def niceName: String = (firstName.is, lastName.is, email.is) match {
  case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
  case (f, _, e) if f.length > 1 => f+" ("+e+")"
  case (_, l, e) if l.length > 1 => l+" ("+e+")"
  case (_, _, e) => e
}

def shortName: String = (firstName.is, lastName.is) match {
case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
case (f, _) if f.length > 1 => f
case (_, l) if l.length > 1 => l
case _ => email.is
}  

def niceNameWEmailLink = <a href={"mailto:"+email.is}>{niceName}</a>  
}

trait MetaMegaProtoUser[ModelType <: MegaProtoUser[ModelType], MyType <: ModelType] extends KeyedMetaMapper[Long, ModelType] { self: MyType =>
  def signupFields: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil
  override def fieldOrder: List[BaseOwnedMappedField[ModelType]] = firstName :: lastName :: email :: locale :: timezone :: password :: Nil  
  
  /**
     * If the 
     */
    def screenWrap: Can[Node] = Empty
    
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
    
    /**
      * Return the URL of the "login" page
      */
    def loginPageURL = "/"+BasePath+"/"+Login
    
    def notLoggedIn_? = !loggedIn_? ;

    lazy val testLogginIn = If(loggedIn_? _, "You must be logged in")
      
    /**
      * The menu item for login (make this "Empty" to disable)
      */
    def loginMenuLoc: Can[Menu] = Full(Menu(Loc("Login", "/user_mgt/login", "Login", If(notLoggedIn_? _, "already logged in. Please logout first."))))
    
    /**
      * The menu item for logout (make this "Empty" to disable)
      */
    def logoutMenuLoc: Can[Menu] = Full(Menu(Loc("Logout", "/user_mgt/logout", "Logout", testLogginIn)))
    
    /**
      * The menu item for creating the user/sign up (make this "Empty" to disable)
      */
    def createUserMenuLoc: Can[Menu] = Full(Menu(Loc("CreateUser", "/user_mgt/sign_up", "Sign Up", If(notLoggedIn_? _, "Please logout first."))))
    
    /**
      * The menu item for lost password (make this "Empty" to disable)
      */
    def lostPasswordMenuLoc: Can[Menu] = Full(Menu(Loc("LostPassword", ("/user_mgt/lost_password", true), "Lost Password", If(notLoggedIn_? _, "Please logout first.")))) // not logged in
    
    /**
       * The menu item for resetting the password (make this "Empty" to disable)
       */
    def resetPasswordMenuLoc: Can[Menu] = Full(Menu(Loc("ResetPassword", "/user_mgt/reset_password", "Reset Password", Hidden, If(notLoggedIn_? _, "Please logout first.")))) //not Logged in
    
    /**
       * The menu item for editing the user (make this "Empty" to disable)
       */
    def editUserMenuLoc: Can[Menu] = Full(Menu(Loc("EditUser", "/user_mgt/edit", "Edit User", testLogginIn)))
    
    /**
      * The menu item for changing password (make this "Empty" to disable)
      */
    def changePasswordMenuLoc: Can[Menu] = Full(Menu(Loc("ChangePassword", "/user_mgt/change_password", "Change Password", testLogginIn)))
    
    /**
      * The menu item for validating a user (make this "Empty" to disable)
      */
    def validateUserMenuLoc: Can[Menu] = Full(Menu(Loc("ValidateUser", ("/user_mgt/validate_user", true), "Validate User", Hidden, If(notLoggedIn_? _, "Please logout first."))))     
        
    lazy val sitemap : List[Menu] = List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc, lostPasswordMenuLoc, resetPasswordMenuLoc,
        editUserMenuLoc, changePasswordMenuLoc, validateUserMenuLoc).flatten(a => a)


    def skipEmailValidation = false
    
    def userMenu: List[Node] = {
      val li = loggedIn_?
      ItemList.filter(i => i.display && i.loggedIn == li).map(i => (<a href={i.path}>{i.name}</a>))
    }
    
    val ItemList: List[MenuItem] = MenuItem("Sign Up", SignUp, false) :: MenuItem("Log In", Login, false) :: 
    MenuItem("Lost Password", LostPassword, false) ::
    MenuItem("", PasswordReset, false) :: MenuItem("Change Password", ChangePassword, true) :: 
    MenuItem("Log Out", Logout, true) :: MenuItem("Edit Profile", Edit, true) :: MenuItem("", ValidateUser, false) :: Nil
    
    def templates: LiftServlet.TemplatePf = {
      case RequestMatcher(_, ParsePath(BasePath :: (w @ SignUp) :: _, _, _), _) if testLoggedIn(w) => () => signup
      case RequestMatcher(_, ParsePath(BasePath :: (w @ Login) :: _, _, _), _) if testLoggedIn(w) => () => login
      case RequestMatcher(_, ParsePath(BasePath :: (w @ LostPassword) :: _, _, _), _) if testLoggedIn(w) => () => lostPassword
      case RequestMatcher(_, ParsePath(BasePath :: (w @ PasswordReset) :: id :: _, _, _), _) if testLoggedIn(w) => () => passwordReset(id)
      case RequestMatcher(_, ParsePath(BasePath :: (w @ ChangePassword) :: _, _, _), _) if testLoggedIn(w) => () => changePassword
      case RequestMatcher(_, ParsePath(BasePath :: (w @ Logout) :: _, _, _), _) if testLoggedIn(w) => () => logout
      case RequestMatcher(_, ParsePath(BasePath :: (w @ Edit) :: _, _, _), _) if testLoggedIn(w) => () => edit
      case RequestMatcher(_, ParsePath(BasePath :: (w @ ValidateUser) :: id :: _, _, _), _) if testLoggedIn(w) => () => validateUser(id)
    }
    
    def requestLoans: List[LoanWrapper] = List(curUser)
    
    val LoggedInUserIdentifier = "$_proto_user_current_user"
    
    def loggedIn_? : Boolean = currentUserId.isDefined
    
    def logUserIn(who: ModelType) {S.set(LoggedInUserIdentifier, who.id.toString); curUser.reset()}
    def logoutCurrentUser {S.unset(LoggedInUserIdentifier); curUser.reset(); S.request.request.getSession.invalidate}
    
    def currentUserId: Can[String] = S.get(LoggedInUserIdentifier)
    
    private val curUser: ThreadLazy[Can[ModelType]] = ThreadLazy(currentUserId.flatMap(id => getSingleton.find(id)))
    
    def currentUser: Can[ModelType] = curUser.get
    
    def signupXhtml(user: ModelType) = (<form method="POST" action={S.action}><table><tr><td colspan="2">Sign Up</td></tr>
    {localForm(user, false)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table></form>)
    
    def signupMailBody(user: ModelType, validationLink: String) =  (<html>
    <head>
    <title>Sign Up Confirmation</title>
    </head>
    <body>
    <p>Dear {user.firstName},
    <br/>
    <br/>
    Click on this link to complete signup
    <br/><a href={validationLink}>{validationLink}</a>
    <br/>
    <br/>
    Thanks
    </p>
    </body>
    </html>)
    
    def signupMailSubject = "Sign up confirmation"
    
    def sendValidationEmail(user: ModelType) {
      val resetLink = S.hostAndPath+"/"+BasePath+"/"+ValidateUser+"/"+user.uniqueId
      val email: String = user.email

      val msgXml = signupMailBody(user, resetLink)

      Mailer.sendMail(From(emailFrom),Subject(signupMailSubject), 
          (To(user.email) :: xmlToMailBodyType(msgXml) :: (bccEmail.toList.map(BCC(_)))) :_* )  
    }
      
    def signup = {
      val theUser: ModelType = create
      val theName = BasePath + SignUp

      def testSignup(ignore: String) {
        theUser.validate match {
          case Nil => S.removeSessionTemplater(theName)
          theUser.validated(skipEmailValidation).uniqueId.reset()
          theUser.save
          if (!skipEmailValidation) {
            sendValidationEmail(theUser)              
            S.notice("You have signed up.  A validation email message will be sent to you.")
          } else {
            S.notice("Welcome")
            logUserIn(theUser)
          }

          S.redirectTo(HomePage)
          
          case xs => S.error(xs)
        }
      }
      
      def innerSignup = bind("user", signupXhtml(theUser), "submit" -> submit("Sign Up", testSignup))

      S.addSessionTemplater(theName, {case RequestMatcher(_, ParsePath(BasePath :: (w @ SignUp) :: _, _, _), _) if testLoggedIn(w) => 
        () => innerSignup})
      innerSignup
    }
    
    def emailFrom = "noreply@"+S.hostName
    
    def bccEmail: Can[String] = Empty
    
    def testLoggedIn(page: String): Boolean =
      ItemList.filter(_.endOfPath == page) match {
        case x :: xs if x.loggedIn == loggedIn_? => true
        case _ => false
      }

    
    def validateUser(id: String) = getSingleton.find(By(uniqueId, id)) match {
      case Full(user) if !user.validated => 
        user.validated(true).uniqueId.reset().save
      S.notice("Account Validated")
      logUserIn(user)
      S.redirectTo(HomePage)  
      
      case _ => S.error("Validation link invalid"); S.redirectTo(HomePage)
    }
    
    def loginXhtml = (<form method="POST" action={S.action}><table><tr><td colspan="2">Log In</td></tr>
    <tr><td>EMail Address</td><td><user:email /></td></tr>
    <tr><td>Password</td><td><user:password /></td></tr>
    <tr><td><a href={"/"+BasePath+"/"+LostPassword}>Recover Password</a></td><td><user:submit /></td></tr></table>
    </form>)
    
    def login = {
      /*
      def testLogin(ignore: String) {
        getSingleton.find(By(email, username)) match {
          case Full(user) if user.validated && user.password.match_?(pwd) => logUserIn(user); S.notice("Logged In"); S.redirectTo(HomePage)
          case _ => S.error("Invalid Username/Password")
        }
      }*/
        
      if (S.post_?) {
        S.param("username").flatMap(username => getSingleton.find(By(email, username))) match {
          case Full(user) if user.validated && user.password.match_?(S.param("password").openOr("*")) => logUserIn(user); S.notice("Logged In"); S.redirectTo(HomePage)
          case Full(user) if !user.validated => S.error("Your account has not been validated.  Please check your email for a validation link.")        
          case _ => S.error("Invalid Username/Password")
        }
      }
      
      bind("user", loginXhtml,
           "email" -> (<input type="text" name="username"/>),
           "password" -> (<input type="password" name="password"/>),
           "submit" -> (<input type="submit" value="Log In"/>))
    }
    
    def lostPasswordXhtml = (<form method="POST" action={S.action}>
    <table><tr><td colspan="2">Enter your email address and we'll email you a link to reset your password</td></tr>
    <tr><td>Email address</td><td><user:email /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
    
     def passwordResetMailBody(user: ModelType, resetLink: String) = (<html>
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
      </html>)
      
      def passwordResetEmailSubject = "Reset Password Request"  
    
        def sendPasswordReset(email: String) {
        getSingleton.find(By(this.email, email)) match {
          case Full(user) if this.validated =>
            user.uniqueId.reset()
          val resetLink = S.hostAndPath+"/"+BasePath+"/"+PasswordReset+"/"+user.uniqueId
          val email: String = user.email

          val msgXml = passwordResetMailBody(user, resetLink)

          Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),  
              (To(user.email) :: xmlToMailBodyType(msgXml) :: (bccEmail.toList.map(BCC(_)))) :_*)
          S.notice("Password Reset Email sent") 
          S.redirectTo(HomePage)
          
          case Full(user) => 
            
            S.notice("Account Validation Re-sent")
            S.redirectTo(HomePage)
          
          case _ => S.error("Email address not found")
        }
      }      
        
    def lostPassword = {
      bind("user", lostPasswordXhtml, "email" -> text("", sendPasswordReset _), "submit" -> <input type="Submit" value="Send It" />)
    }
    
    def passwordResetXhtml = (<form method="POST" action={S.action}>
    <table><tr><td colspan="2">Reset your password</td></tr>
    <tr><td>Enter your new password</td><td><user:pwd/></td></tr>
    <tr><td>Enter your new password (repeat)</td><td><user:pwd/></td></tr>
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
    
    def passwordReset(id: String) = getSingleton.find(By(uniqueId, id)) match {
      case Full(user) => 
        def finishSet(ignore: String) {
          user.validate match {
            case Nil => S.notice("Password Changed"); user.save; logUserIn(user); S.redirectTo(HomePage)
            case xs => S.error(xs)
          }
        }
      user.uniqueId.reset().save
      
      bind("user", passwordResetXhtml,
           "pwd" -> password_*("",(p: List[String]) => user.password.setList(p)),
           "submit" -> submit("Set Password", finishSet))
      case _ => S.error("Password reset link invalid"); S.redirectTo(HomePage)
    }
    
    def changePasswordXhtml = (<form method="POST" action={S.action}>
    <table><tr><td colspan="2">Change Password</td></tr>
    <tr><td>Old Password</td><td><user:old_pwd /></td></tr>
    <tr><td>New Password</td><td><user:new_pwd /></td></tr>
    <tr><td>New Password (repeat)</td><td><user:new_pwd /></td></tr>
    <tr><td>&nbsp;</td><td><user:submit /></td></tr>
    </table>
    </form>)
    
    def changePassword = {
      val user = currentUser.open_! // we can do this because the logged in test has happened
      var oldPassword = ""
      var newPassword: List[String] = Nil
      
      def testAndSet(ignore: String) {
        if (!user.password.match_?(oldPassword)) S.error("Wrong old password")
        else {
          user.password.setFromAny(newPassword)
          user.validate match {
            case Nil => user.save; S.notice("Password Changed"); S.redirectTo(HomePage)
            case xs => S.error(xs)
          }
        }
      }
      
      bind("user", changePasswordXhtml,
           "old_pwd" -> S.password("", oldPassword = _), 
           "new_pwd" -> password_*("", LFuncHolder(newPassword = _)), 
           "submit" -> submit("Change", testAndSet _))
    }
    
    def editXhtml(user: ModelType) = (<form method="POST" action={S.action}>
    <table><tr><td colspan="2">Edit</td></tr>
    {localForm(user, true)}
    <tr><td>&nbsp;</td><td><user:submit/></td></tr>
    </table>
    </form>)
    
    def edit = {
      val theUser: ModelType = currentUser.open_! // we know we're logged in
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

      S.addSessionTemplater(theName, {case RequestMatcher(_, ParsePath(BasePath :: (w @ Edit) :: _, _, _), _) if testLoggedIn(w) => 
        () => innerEdit})
      innerEdit
    }
    
    def logout = {
      logoutCurrentUser
      S.redirectTo(HomePage)
    }
    
    private def localForm(user: ModelType, ignorePassword: Boolean): NodeSeq = signupFields.
    map(fi => getSingleton.getActualBaseField(user, fi)).
    filter(f => !ignorePassword || (f match {
      case f: MappedPassword[ModelType] => false
      case _ => true
    })).
    map(f => (<tr><td>{f.displayName}</td><td>{f.toForm}</td></tr>) )
    
    protected implicit def nodeSeqToOption(in: NodeSeq): Can[NodeSeq] = 
      screenWrap.map{
        theDoc => 
          val rw = new RewriteRule {
            override def transform(n: Node) = n match {
              case e @ (<bind />) if "lift" == e.prefix => in
              case _ => n
            }
          }
        (new RuleTransformer(rw))(theDoc)
      } or Full(in)  
}

trait MegaProtoUser[T <: MegaProtoUser[T]] extends ProtoUser[T] { self: T =>
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }
  
  object validated extends MappedBoolean[T](this) {
    override def defaultValue = false
  }

  object locale extends MappedLocale[T](this)
  
  object timezone extends MappedTimeZone[T](this)

}
