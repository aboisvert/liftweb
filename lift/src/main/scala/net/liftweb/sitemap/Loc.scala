package net.liftweb.sitemap

/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text}

/**
 * A menu location
 */
class Loc(val name: String, val link: Loc.Link, val text: Loc.LinkText, val stuff: List[Loc.LocStuff]) {
  override def toString = "Loc("+name+", "+link+", "+text+", "+stuff+")"

  def testAccess: Either[Boolean, Can[LiftResponse]] = {
    def testStuff(what: List[Loc.LocStuff]): Either[Boolean, Can[LiftResponse]] = what match {
      case Nil => Left(true)

      case Loc.If(test, msg) :: xs =>
        if (!test()) Right(Full(msg()))
        else testStuff(xs)

      case Loc.Unless(test, msg) :: xs =>
        if (test()) Right(Full(msg()))
        else testStuff(xs)

      case x :: xs => testStuff(xs)
    }

    testStuff(stuff) match {
      case Left(true) => _menu.testParentAccess
      case x => x
    }
  }

  private def findTitle(lst: List[Loc.LocStuff]): Can[Loc.Title] = lst match {
    case Nil => Empty
    case (t : Loc.Title) :: xs => Full(t)
    case _ => findTitle(lst.tail)
  }

  /**
   * The title of the location
   */
  def title: NodeSeq = findTitle(stuff).map(_.title()) openOr text.text()

  private[sitemap] def setMenu(p: Menu)
  {
    _menu = p
    p.siteMap.addLoc(this)
  }

  private var _menu: Menu = _
  def menu = _menu

  private def testAllStuff(what: List[Loc.LocStuff], req: RequestState): Boolean = {
    what match {
      case Nil => true
      case (x: Loc.Test) :: xs =>
        if (!x.test(req)) false
        else testAllStuff(xs, req)

      case x :: xs => testAllStuff(xs, req)
    }
  }

  def doesMatch_?(req: RequestState): Boolean =
  if (link.isDefinedAt( req ) ) {
    link(req) match {
      case Full(x) if testAllStuff(stuff, req) => x
      case Full(x) => false
      case x => x.openOr(false)
    }
  } else false

  def buildMenu: CompleteMenu = {
    val theKids = _menu.kids.toList.flatMap(_.loc.buildItem(Nil, false, false))
    
    CompleteMenu(_menu.buildUpperLines(_menu, _menu, theKids))
  }
                                            
  private[sitemap] def buildItem(kids: List[MenuItem], current: Boolean, path: Boolean): Can[MenuItem] =
  (hidden, testAccess) match {
    case (false, Left(true)) =>
      link.createLink(Nil).map(t =>
        MenuItem(text.text(),t, kids, current, path,
                 stuff.flatMap {
            case v: Loc.LocInfo[(T forSome {type T})] => v()
            case _ =>  Empty
          }
        ))

    case _ => Empty
  }

  private def hidden = stuff.contains(Loc.Hidden)
  
  private lazy val groupSet: Set[String] = 
   Set(stuff.flatMap{case s: Loc.LocGroup => s.group case _ => Nil} :_*)
  
  def inGroup_?(group: String): Boolean = groupSet.contains(group)
}

/**
 * The Loc companion object, complete with a nice constructor
 */
object Loc {
  type FailMsg = () => LiftResponse

  /**
   * Create a Loc (Location) instance
   *
   * @param name -- the name of the location.  This must be unique across your entire sitemap.
   * It's used to look up a menu item in order to create a link to the menu on a page.
   * @param link -- the Link to the page
   * @param text -- the text to display when the link is displayed
   * @param params -- access test, title calculation, etc.
   *
   */
  def apply(name: String,
            link: Link,
            text: LinkText,
            params: LocStuff*): Loc = new Loc(name, link, text, params.toList)

  /**
   * Unapply to do pattern matching against a Loc.
   * (name, link_uri, link_text)
   */
  /*
   def unapplySeq(loc: Loc) : Option[(String, String, String)] =
   Some((loc.name, loc.link.uri, loc.text.text()))
   */

  trait LocStuff
  /**
   * A title for the page.  A function that calculates the title... useful
   * if the title of the page is dependent on current state
   */
  case class Title(title: () => NodeSeq) extends LocStuff

  /**
   * If this parameter is included, the item will not be visible in the menu, but
   * will still be accessable.
   */
  case object Hidden extends LocStuff
  
/**
* If the Loc is in a group (or groups) like "legal" "community" etc.
* the groups can be specified and recalled at the top level
*/
case class LocGroup(group: String*) 

  /**
   * If the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class If(test: () => Boolean, failMsg: FailMsg) extends LocStuff

  /**
   * Unless the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class Unless(test: () => Boolean, failMsg: FailMsg) extends LocStuff

  /**
   * Tests to see if the request actually matches the requirements for access to
   * the page.  For example, if a parameter is missing from the request, this
   * is a good way to restrict access to the page.
   */
  case class Test(test: RequestState => Boolean) extends LocStuff

  /**
   * What's the link text.
   */
  case class LinkText(text: () => NodeSeq)

  /**
   * This defines the Link to the Loc.
   *
   * @param uri -- the relative (to parent menu item) or absolute path
   * to match for this Loc. <br />
   * "/foo" -- match the "foo" file <br/>
   * "foo" -- match the foo file in the directory defined by the parent Menu
   * @param matchOnPrefix -- false -- absolute match.  true -- match anything
   * that begins with the same path.  Useful for opening a set of directories
   * (for example, help pages)
   * @param create -- create a URL based on incoming parameters (NO IMPLEMENTED **TODO**)
   */
  class Link(val buildUri: List[String], val matchHead_? : Boolean) extends PartialFunction[RequestState, Can[Boolean]] {
    def this(b: List[String]) = this(b, false)

    def isDefinedAt(req: RequestState): Boolean =
    if (matchHead_?) req.path.partPath.take(buildUri.length) == buildUri
    else buildUri == req.path.partPath

    def apply(in: RequestState): Can[Boolean] = if (isDefinedAt(in)) Full(true)
    else throw new MatchError("Failed for Link "+buildUri)


    def createLink(params: Seq[(String, String)]): Can[String] =
    Full(buildUri.mkString("/", "/", ""))

  }

  /**
   * A companion object to create some variants on Link
   */
  object Link {
    def apply(urlLst: List[String], matchHead_? : Boolean, url: String) =
    new Link(urlLst, matchHead_?) {
    override def createLink(params: Seq[(String, String)]): Can[String] =
    Full(url)
  }
    
  }

object ExtLink {
  def apply(url: String) = new Link(List(randomString(20)), false) {
    override def createLink(params: Seq[(String, String)]): Can[String] =
    Full(url)
  }
}

  trait LocInfoVal[T] {
    def value: T
  }

  trait LocInfo[T] extends LocStuff {
    def apply(): Can[LocInfoVal[T]]
  }

  def alwaysTrue(a: RequestState) = true
  def retString(toRet: String)(other: Seq[(String, String)]) = Full(toRet)

implicit def nodeSeqToLinkText(in: => NodeSeq): LinkText = LinkText(() => in)
  implicit def strToLinkText(in: => String): LinkText = LinkText(() => Text(in))
  implicit def strLstToLink(in: Seq[String]): Link = new Link(in.toList)
  implicit def strPairToLink(in: (Seq[String], Boolean)): Link = new Link(in._1.toList, in._2)
  implicit def strToFailMsg(in: String): FailMsg = 
  f(RedirectWithState(LiftRules.siteMapFailRedirectLocation.
                      mkString("/", "/", ""),
                      RedirectState(Empty, in -> NoticeType.Error)))
  implicit def redirectToFailMsg(in: RedirectResponse): FailMsg = f(in)

  def f(in: String): () => String = () => in
  def f(in: RedirectResponse): () => RedirectResponse = () => in
}



case class CompleteMenu(lines: Seq[MenuItem]) {
  lazy val breadCrumbs: Seq[MenuItem] = lines.flatMap(_.breadCrumbs)
}

case class MenuItem(text: NodeSeq, uri: String,  kids: Seq[MenuItem],
                    current: Boolean,
                    path: Boolean, 
                    info: List[Loc.LocInfoVal[(T forSome {type T})]])
{
  def breadCrumbs: Seq[MenuItem] = if (!path) Nil
  else this :: kids.toList.flatMap(_.breadCrumbs)
}
