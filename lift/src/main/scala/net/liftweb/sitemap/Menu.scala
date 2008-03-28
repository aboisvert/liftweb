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

case class Menu(page: Loc, kids: Menu*) extends HasKids {
  private[sitemap] var _parent: Can[HasKids] = Empty
  
  private[sitemap] def init {
    kids.foreach(_._parent = Full(this))
    kids.foreach(_.init)
    page.setMenu(this)
  }
  
  private[sitemap] def validate {
    _parent.foreach(p => if (p.isRoot_?) throw new SiteMapException("Menu items with root location (\"/\") cannot have children"))
    kids.foreach(_.validate)
  }
  
  private[sitemap] def testParentAccess: (Boolean, Can[ResponseIt]) = _parent match {
    case Full(p) => p.testAccess
    case _ => (true, Empty)
  }
  
  override private[sitemap] def testAccess: (Boolean, Can[ResponseIt]) = page.testAccess
  
  override def isRoot_? = page.isRoot_?
      
  def isAbsolute_? = page.isAbsolute_?

  def findLoc(orgPath: ParsePath, path: List[String], req: RequestState): Can[Loc] = {
    if (page.doesMatch_?(path, req)) Full(page)
    else page.pathMatch(path) match {
      case 0 => first(kids.filter(_.isAbsolute_?).toList)(_.findLoc(orgPath, orgPath.path, req))
      case n =>
      val p2 = path.drop(n)
      first(kids.filter(!_.isAbsolute_?).toList)(_.findLoc(orgPath, p2, req)) or 
         first(kids.filter(_.isAbsolute_?).toList)(_.findLoc(orgPath, orgPath.path, req))
    }
  }
  
  def buildThisLine(loc: Loc) = {
    val menuList = _parent.map(_.kids.toList) openOr List(this)
    MenuLine(menuList.flatMap{
      mi =>
      val p = mi.page
      val same = loc eq p
      p.buildItem(same, same).toList
    })
  }
  
  def buildChildLine = MenuLine(kids.toList.flatMap(m => m.page.buildItem(false, false).toList))
  override def buildUpperLines: List[MenuLine] = _parent match {
    case Full(p) => p.buildUpperLines ::: p.buildAboveLine(this)
    case _ => Nil
  }
  
  override def buildAboveLine(path: Menu): List[MenuLine] = _parent match {
    case Full(p) => List(MenuLine(p.kids.toList.flatMap(m => m.page.buildItem(false, m eq path).toList)))
    case _ => Nil
  }
}

class SiteMapException(msg: String) extends Exception(msg)
