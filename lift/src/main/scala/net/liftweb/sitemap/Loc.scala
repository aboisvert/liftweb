package net.liftweb.sitemap

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import net.liftweb.http._

class Loc(val name: String) {
  def testAccess: Option[(String, String)] = None
}

object Loc {
  def apply(name: String,
      link: Link,
      text: LinkText,
      params: LocStuff*): Loc = new Loc(name)

  abstract class LocStuff
  case class Title(title: () => String) extends LocStuff 
  case class If(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Unless(test: () => boolean, failMsg: FailMsg) extends LocStuff
  case class Test(test: ((String, List[String], RequestType, HttpServletRequest)) => boolean) extends LocStuff
  case class LinkText(text: () => String)
  case class Link(uri: String, test: ((String, List[String], RequestType, HttpServletRequest)) => boolean,
                 create: (Seq[(String, String)]) => String)
  case class FailMsg(msg: () => String)
  
  private def alwaysTrue(a: (String, List[String], RequestType, HttpServletRequest)) = true
  private def retString(toRet: String)(other: Seq[(String, String)]) = toRet
  
  implicit def strToLinkText(in: String): LinkText = LinkText(f(in))
  implicit def strToLink(in: String): Link = Link(in, &alwaysTrue, &retString(in))
  implicit def strToFailMsg(in: String): FailMsg = FailMsg(f(in))
  
  //implicit def strToFofStr(in: String): () => String = f(in)
  def f(in: String): () => String = () => in
}

