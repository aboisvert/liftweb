package net.liftweb.sitemap

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import net.liftweb.http._
import net.liftweb.util._
import Helpers._

class Loc(val name: String, val link: Loc.Link, val text: Loc.LinkText, val stuff: List[Loc.LocStuff]) {
  def testAccess: Option[RedirectWithMessage] = {
    first(stuff)(s =>
     s match {
       case Loc.If(test, msg) if (!test()) => Some(msg.msg())
       case Loc.Unless(test, msg) if test() => Some(msg.msg())
       case _ => None
     }
    ) orElse _menu.testParentAccess
  }
  
  private def findTitle(lst: List[Loc.LocStuff]): Option[Loc.Title] = lst match {
    case Nil => None
    case (t : Loc.Title) :: xs => Some(t)
    case _ => findTitle(lst.tail)
  }
  
  def title: String = findTitle(stuff).map(_.title()) getOrElse text.text()
  
  def isRoot_? = link.isRoot_?
  private[sitemap] def setMenu(p: Menu) {_menu = p}
  private var _menu: Menu = _
  def menu = _menu

  private def testAllStuff(what: List[Loc.LocStuff], path: ParsePath, req: RequestState, httpReq: HttpServletRequest): boolean = {
    what match {
      case Nil => true
      case (x: Loc.Test) :: xs =>
      if (!x.test(path, req, httpReq)) false
      else testAllStuff(xs, path, req, httpReq) 
      
      case x :: xs => testAllStuff(xs, path, req, httpReq)
    }
  }
  
  def doesMatch_?(path: List[String], req: RequestState, httpReq: HttpServletRequest): boolean = 
    link.path.path == path && link.test(req.path, req, httpReq) && testAllStuff(stuff, req.path, req, httpReq)
  
  /*
  def doesPathlessMatch_?(path: List[String], req: RequestState, httpReq: HttpServletRequest): boolean = {
     link.test(path, req, httpReq) && testAllStuff(stuff, path, req, httpReq)
  }*/
  
  def isAbsolute_? = link.isAbsolute_?  
  def pathMatch(path: List[String]): int = {
    val mod = if (link.path.endSlash) 1 else 0
    val len = link.path.path.length - mod
    val p2 = path.take(len)
    if (p2 == link.path.path.dropRight(mod)) len else 0
  }
  
  def buildMenu: CompleteMenu = CompleteMenu(_menu.buildUpperLines ::: List(_menu.buildThisLine(this)) ::: List(_menu.buildChildLine))
  
  private[sitemap] def buildItem(current: boolean, path: boolean) = {
    if (hidden || testAccess.isDefined) None
    else link.create(Nil).map(t => MenuItem(text.text(),t , current, path))
  }
  
  private def hidden = stuff.contains(Loc.Hidden)
}

object Loc {
  def apply(name: String,
      link: Link,
      text: LinkText,
      params: LocStuff*): Loc = new Loc(name, link, text, params.toList)

  abstract class LocStuff
  case class Title(title: () => String) extends LocStuff 
  case object Hidden extends LocStuff
  case class If(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Unless(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Test(test: ((ParsePath, RequestState, HttpServletRequest)) => boolean) extends LocStuff
  case class LinkText(text: () => String)
  case class Link(uri: String, test: ((ParsePath, RequestState, HttpServletRequest)) => boolean,
                 create: (Seq[(String, String)]) => Option[String]) {
    val path = RequestState.parsePath(uri)
    
    def isRoot_? = uri == "/"
    def isAbsolute_? = path.absolute
  }
  case class FailMsg(msg: () => RedirectWithMessage)
  
  private def alwaysTrue(a: (ParsePath, RequestState, HttpServletRequest)) = true
  private def retString(toRet: String)(other: Seq[(String, String)]) = Some(toRet)
  
  implicit def strToLinkText(in: String): LinkText = LinkText(f(in))
  implicit def strToLink(in: String): Link = Link(in, alwaysTrue _, retString(in) _)
  implicit def strToFailMsg(in: String): FailMsg = FailMsg(f(RedirectWithMessage("/", in)))
  implicit def redirectToFailMsg(in: RedirectWithMessage): FailMsg = FailMsg(f(in))
  
  //implicit def strToFofStr(in: String): () => String = f(in)
  def f(in: String): () => String = () => in
  def f(in: RedirectWithMessage): () => RedirectWithMessage = () => in
}

case class RedirectWithMessage(to: String, msg: String)

case class CompleteMenu(lines: List[MenuLine]) {
  private val _breadCrumbs = Lazy(lines.flatMap(_.breadCrumbs))
  def breadCrumbs: List[MenuItem] = _breadCrumbs.get
}
case class MenuLine(items: List[MenuItem]) {
  private[sitemap] def breadCrumbs: List[MenuItem] = items.filter(_.path)
}
case class MenuItem(text: String, uri: String, current: boolean, path: boolean)

