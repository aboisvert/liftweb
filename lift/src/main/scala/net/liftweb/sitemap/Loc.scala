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

import net.liftweb.http._
import net.liftweb.util._
import Helpers._

/**
  * A menu location
  */
class Loc(val name: String, val link: Loc.Link, val text: Loc.LinkText, val stuff: List[Loc.LocStuff]) {
  override def toString = "Loc("+name+", "+link+", "+text+", "+stuff+")"
    
  def testAccess: (Boolean, Can[ResponseIt]) = {
    def testStuff(what: List[Loc.LocStuff]): (Boolean, Can[ResponseIt]) = what match {
      case Nil => (true, Empty)
      
      case Loc.If(test, msg) :: xs =>
      if (!test()) (false, Full(msg()))
      else testStuff(xs)
      
      case Loc.Unless(test, msg) :: xs =>
      if (test()) (false, Full(msg()))
      else testStuff(xs)
      
      case x :: xs => testStuff(xs)
    }
    
    testStuff(stuff) match {
      case (true, _) => _menu.testParentAccess
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
  def title: String = findTitle(stuff).map(_.title()) openOr text.text()
  
  def isRoot_? = link.isRoot_?
  private[sitemap] def setMenu(p: Menu) {_menu = p}
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
  
  def doesMatch_?(path: List[String], req: RequestState): Boolean = 
    link.matchPath(path) && link.test(req) && testAllStuff(stuff, req)
  
  
  def isAbsolute_? = link.isAbsolute_?  
  def pathMatch(path: List[String]): Int = {
    val mod = if (link.path.endSlash) 1 else 0
    if (link.matchOnPrefix) {if (path.take(link.path.partPath.length) == link.path.partPath) path.length else 0}
    else {
    val len = link.path.partPath.length - mod
    val p2 = path.take(len)
    if (p2 == link.path.partPath.dropRight(mod)) len else 0}
  }
  
  def buildMenu: CompleteMenu = CompleteMenu(_menu.buildUpperLines ::: List(_menu.buildThisLine(this)) ::: List(_menu.buildChildLine))
  
  private[sitemap] def buildItem(current: Boolean, path: Boolean) =
    if (hidden || !testAccess._1) Empty
    else link.create(Nil).map(t => MenuItem(text.text(),t , current, path, 
    stuff.flatMap{case v: Loc.LocInfo[(T forSome {type T})] => v() case _ =>  Empty}))
  
  private def hidden = stuff.contains(Loc.Hidden)
}

/**
  * The Loc companion object, complete with a nice constructor
  */
object Loc {
  type FailMsg = () => ResponseIt
  
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
  def unapplySeq(loc: Loc) : Option[(String, String, String)] =
    Some((loc.name, loc.link.uri, loc.text.text()))

  trait LocStuff
  /**
    * A title for the page.  A function that calculates the title... useful
    * if the title of the page is dependent on current state
    */
  case class Title(title: () => String) extends LocStuff 
  
  /**
    * If this parameter is included, the item will not be visible in the menu, but
    * will still be accessable.
    */
  case object Hidden extends LocStuff
  
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
  case class LinkText(text: () => String)
  
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
    * @param test -- a function to do further testing
    * @param create -- create a URL based on incoming parameters (NO IMPLEMENTED **TODO**)
    */
  case class Link(uri: String, matchOnPrefix: Boolean, test: RequestState => Boolean,
                 create: (List[(String, String)]) => Can[String]) {
    lazy val path = RequestState.parsePath(uri)
    
    def isRoot_? = uri == "/"
    def isAbsolute_? = path.absolute
        
    def matchPath(toMatch: List[String]): Boolean = if (!matchOnPrefix) path.partPath == toMatch else {
      val ret = toMatch.take(path.partPath.length) == path.partPath
      ret
    }
  }
  
  /**
    * A companion object to create some variants on Link
    */
  object Link {
    def apply(uri: String, matchOnPrefix: Boolean): Link = new Link(uri, matchOnPrefix, alwaysTrue _, retString(uri) _ )
    def apply(uri: String): Link = new Link(uri, false, alwaysTrue _, retString(uri) _ )
  }
  
  
  trait LocInfoVal[T] {
    def value: T
  }
  
  trait LocInfo[T] extends LocStuff { 
     def apply(): Can[LocInfoVal[T]]
  }
  
  def alwaysTrue(a: RequestState) = true
  def retString(toRet: String)(other: List[(String, String)]) = Full(toRet)
  
  implicit def strToLinkText(in: => String): LinkText = LinkText(() => in)
  implicit def strToLink(in: String): Link = Link(in, false, alwaysTrue _, retString(in) _)
  implicit def strPairToLink(in: (String, Boolean)): Link = Link(in._1, in._2, alwaysTrue _, retString(in._1) _)
  implicit def strToFailMsg(in: String): FailMsg = f(RedirectWithState("/", RedirectState(Empty, in -> NoticeType.Error)))
  implicit def redirectToFailMsg(in: RedirectResponse): FailMsg = f(in)
  
  def f(in: String): () => String = () => in
  def f(in: RedirectResponse): () => RedirectResponse = () => in
}


case class CompleteMenu(lines: List[MenuLine]) {
  lazy val breadCrumbs: List[MenuItem] = lines.flatMap(_.breadCrumbs)
}
case class MenuLine(items: List[MenuItem]) {
  private[sitemap] def breadCrumbs: List[MenuItem] = items.filter(_.path)
}



case class MenuItem(text: String, uri: String, current: Boolean, path: Boolean, info: List[Loc.LocInfoVal[(T forSome {type T})]])
