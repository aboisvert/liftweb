package com.skittr.snippet

import scala.xml._
import net.liftweb.http._
import net.liftweb.http.S._
import com.skittr.model._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import com.skittr.actor._

class UserMgt {
  def login_panel(xhtml: Group): NodeSeq = {
    <h1>acct</h1> ++
    (if (logged_in_?) {
      <ul><li><a href="/logout">log out</a></li></ul>
    } else {
        var username = ""
        var pwd = ""
        def testPwd(ignore: String) {
           User.find(By(User.name, username)).filter(_.password.match_?(pwd)).map{
             u => S.set("user_name", u.name)
             S.redirectTo("/")
           }.getOrElse(S.error("Invalid Username/Password"))
        }
      <form method="POST" action={S.request.uri}>
      <table>
      <tr><td>name:</td><td>{S.text(username=_)}</td></tr>
      <tr><td>pwd:</td><td>{S.password(pwd=_)}</td></tr>
      <tr><td><a href="/new_acct">new acct</a></td><td>{S.submit(&testPwd, Val("login"))}</td></tr>
      </table>
      </form>
    })
  }
  
  def new_account: NodeSeq = {
    if (logged_in_?) {S.error("Can't create new account if you're logged in"); S.redirectTo("/")}
    else {
      val invokedAs = S.invokedAs
      val theUser = new User
      def newAccount(ignore: NodeSeq): NodeSeq = {
        def saveMe(in: List[String]): boolean = {
          // validate the user
          val issues = theUser.validate
          
          // if there are no issues, set the friendly name, destroy the token, log the user in and send them to the home page
          if (issues.isEmpty) {
            theUser.save
            S.set("user_name", theUser.name)
            S.notice("welcome to skittr")
            redirectTo("/")
          }

          // This method tells lift that if we get another call to the same snippet during a page
          // reload, don't create a new instance, just invoke the innerAccept function which has
          // "user" bound to it
          S.mapSnippet(invokedAs, newAccount)
          
          // whoops... we have issues, display them to the user and continue loading this page
          error(issues)          
          true
        }
        <form method="POST" action={S.request.uri}>
        <table>{
          theUser.toForm(saveMe)
        }
        <tr><td>&nbsp;</td><td><input type="submit" value="Create New Account"/></td></tr></table>
        </form>
      }
      
      newAccount(Text(""))
    }
  }
  
  def logout: NodeSeq = {
    S.unset("user_name")
    S.redirectTo("/")
  }
  
  private def friendList(user: UserIdInfo): NodeSeq = <ul>{
    user.friends.map(f => <li><a href={"/user/"+f}>{f}</a></li>)
  }</ul> ++ (
      (for (val curUser <- S.get("user_name");
           val ua <- UserList.find(curUser);
           val userInfo <- (ua !? (400L, GetUserIdAndName)) match {case Some(u: UserIdInfo) => Some(u) ; case _ => None}) yield {
        if (userInfo.friends.contains(user.name)) <a href={"/unfriend/"+user.name}>Unfriend</a>
        else <a href={"/friend/"+user.name}>Befriend</a>
      }
      ) getOrElse Text(""))
  
  def show_user(xhtml: Group): NodeSeq = {
    (for (val userName <- S.param("user");
         val userActor <- UserList.find(userName);
         val user <- (userActor !? (400L, GetUserIdAndName)) match {case Some(u: UserIdInfo) => Some(u) ; case _ => None};
         val messages <- (userActor !? (400L, GetMessages)) match {case Some(m : Messages) => Some(m.messages); case _ => None}) yield {
      bind("sk", xhtml, "username" -> (user.name+" -> "+user.fullName), "content" -> friendList(user)) ++ 
        messages.flatMap{
        msg => 
        Helpers.bind("sk", xhtml, "username" -> (msg.who+" @ "+toInternetDate(msg.when)), "content" -> msg.text)
      }
    }) getOrElse {S.error("User "+(S.param("user") getOrElse "")+" not found"); S.redirectTo("/")}
  }
  
  def watch_or_show(xhtml: Group): NodeSeq = {
   (for (val userName <- S.get("user_name")) yield {
    <lift:controller type="watch_user" name={userName}>
    {
      xhtml.nodes
    }
    </lift:controller>
    }) getOrElse {
      Helpers.bind("sk", xhtml, "username" -> <a href="/new_acct">Create a New Account</a>, 
          "content" -> <span>See what others are up to:<ul>{
            UserList.randomUsers(40).flatMap {
              u =>
              <li><a href={"/user/"+u}>{u}</a></li>
            }
          }</ul></span>)
    }
  }
 
  def friend: NodeSeq = {
    (for (val userName <- S.get("user_name");
          val userActor <- UserList.find(userName);
          val toFriend <- S.param("user")) yield {
      S.notice("You've add "+toFriend+" your your list of friends")
       userActor ! AddFriend(toFriend)      
    }) getOrElse {
      S.error("Unable to friend")
    }

    S.redirectTo("/")
  }
  
  def unfriend: NodeSeq = {
    (for (val userName <- S.get("user_name");
          val userActor <- UserList.find(userName);
          val toUnfriend <- S.param("user")) yield {
      S.notice("You've removed "+toUnfriend+" from your list of friends")
       userActor ! RemoveFriend(toUnfriend)      
    }) getOrElse {
      S.error("Unable to unfriend")
    }

    S.redirectTo("/")
  }
  
  def random(xhtml: Group): NodeSeq = {
    Helpers.bind("sk", xhtml, "username" -> "A Random List of Users", 
        "content" -> <span>See what others are up to:<ul>{
          UserList.randomUsers(40).flatMap {
            u =>
            <li><a href={"/user/"+u}>{u}</a></li>
          }
        }</ul></span>)    
  }
  
  def cur_name:  MetaData = new UnprefixedAttribute("name", S.param("user").getOrElse(""), Null)
  
  def logged_in_? = S.get("user_name").isDefined
}
