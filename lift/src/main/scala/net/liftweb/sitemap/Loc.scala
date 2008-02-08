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
  override def toString = "Loc("+name+", "+link+", "+text+", "+stuff+")"
    
  def testAccess: Can[RedirectWithMessage] = {
    first(stuff)(s =>
     s match {
       case Loc.If(test, msg) if (!test()) => Full(msg.msg())
       case Loc.Unless(test, msg) if test() => Full(msg.msg())
       case _ => Empty
     }
    ) or _menu.testParentAccess
  }
  
  private def findTitle(lst: List[Loc.LocStuff]): Can[Loc.Title] = lst match {
    case Nil => Empty
    case (t : Loc.Title) :: xs => Full(t)
    case _ => findTitle(lst.tail)
  }
  
  def title: String = findTitle(stuff).map(_.title()) openOr text.text()
  
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
  
  def doesMatch_?(path: List[String], req: RequestState, httpReq: HttpServletRequest): Boolean = 
    link.matchPath(path) && link.test(req.path, req, httpReq) && testAllStuff(stuff, req.path, req, httpReq)
  
  /*
  def doesPathlessMatch_?(path: List[String], req: RequestState, httpReq: HttpServletRequest): boolean = {
     link.test(path, req, httpReq) && testAllStuff(stuff, path, req, httpReq)
  }*/
  
  def isAbsolute_? = link.isAbsolute_?  
  def pathMatch(path: List[String]): Int = {
    val mod = if (link.path.endSlash) 1 else 0
    if (link.matchOnPrefix) {if (path.take(link.path.path.length) == link.path.path) path.length else 0}
    else {
    val len = link.path.path.length - mod
    val p2 = path.take(len)
    if (p2 == link.path.path.dropRight(mod)) len else 0}
  }
  
  def buildMenu: CompleteMenu = CompleteMenu(_menu.buildUpperLines ::: List(_menu.buildThisLine(this)) ::: List(_menu.buildChildLine))
  
  private[sitemap] def buildItem(current: Boolean, path: Boolean) =
    if (hidden || testAccess.isDefined) Empty
    else link.create(Nil).map(t => MenuItem(text.text(),t , current, path, stuff.flatMap{case v: Loc.LocInfo[Any] => v() case _ =>  Empty}))
  
  private def hidden = stuff.contains(Loc.Hidden)
}

object Loc {
  def apply(name: String,
      link: Link,
      text: LinkText,
      params: LocStuff*): Loc = new Loc(name, link, text, params.toList)
  def unapplySeq(loc: Loc) : Option[(String, String, String, Seq[LocStuff])] =
    Some((loc.name, loc.link.uri, loc.text.text(), loc.stuff))

  trait LocStuff
  case class Title(title: () => String) extends LocStuff 
  case object Hidden extends LocStuff
  case class If(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Unless(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Test(test: ((ParsePath, RequestState, HttpServletRequest)) => Boolean) extends LocStuff
  case class LinkText(text: () => String)
  case class Link(uri: String, matchOnPrefix: Boolean, test: ((ParsePath, RequestState, HttpServletRequest)) => Boolean,
                 create: (Seq[(String, String)]) => Can[String]) {
    val path = RequestState.parsePath(uri)
    
    def isRoot_? = uri == "/"
    def isAbsolute_? = path.absolute
        
    def matchPath(toMatch: List[String]): Boolean = if (!matchOnPrefix) path.path == toMatch else {
      val ret = toMatch.take(path.path.length) == path.path
      ret
    }
  }
  case class FailMsg(msg: () => RedirectWithMessage)
  
  trait LocInfoVal[T] {
    def value: T
  }
  
  trait LocInfo[T] extends LocStuff { //  with Function0[Can[LocInfoVal[T]]] {
     def apply(): Can[LocInfoVal[T]]
  }
  
  private def alwaysTrue(a: (ParsePath, RequestState, HttpServletRequest)) = true
  private def retString(toRet: String)(other: Seq[(String, String)]) = Full(toRet)
  
  implicit def strToLinkText(in: String): LinkText = LinkText(f(in))
  implicit def strToLink(in: String): Link = Link(in, false, alwaysTrue _, retString(in) _)
  implicit def strPairToLink(in: (String, Boolean)): Link = Link(in._1, in._2, alwaysTrue _, retString(in._1) _)
  implicit def strToFailMsg(in: String): FailMsg = FailMsg(f(RedirectWithMessage("/", in)))
  implicit def redirectToFailMsg(in: RedirectWithMessage): FailMsg = FailMsg(f(in))
  
  //implicit def strToFofStr(in: String): () => String = f(in)
  def f(in: String): () => String = () => in
  def f(in: RedirectWithMessage): () => RedirectWithMessage = () => in
}

case class RedirectWithMessage(to: String, msg: String)

case class CompleteMenu(lines: List[MenuLine]) {
  lazy val breadCrumbs: List[MenuItem] = lines.flatMap(_.breadCrumbs)
}
case class MenuLine(items: List[MenuItem]) {
  private[sitemap] def breadCrumbs: List[MenuItem] = items.filter(_.path)
}
case class MenuItem(text: String, uri: String, current: Boolean, path: Boolean, info: List[Loc.LocInfoVal[_]])

