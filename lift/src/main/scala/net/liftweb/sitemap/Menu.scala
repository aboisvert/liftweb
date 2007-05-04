package net.liftweb.sitemap

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

   import net.liftweb.http._
   import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}
   import net.liftweb.util._
   import Helpers._
   
case class Menu(page: Loc, kids: Menu*) {
  private var _parent: Option[Menu] = None
  
  private[sitemap] def init {
    kids.foreach(_._parent = Some(this))
    kids.foreach(_.init)
    page.setMenu(this)
  }
  
  private[sitemap] def validate {
    _parent.foreach(p => if (p.isRoot_?) throw new SiteMapException("Menu items with root location (\"/\") cannot have children"))
    kids.foreach(_.validate)
  }
  
  private[sitemap] def testParentAccess: Option[RedirectWithMessage] = _parent.flatMap(_.testAccess)
  private[sitemap] def testAccess: Option[RedirectWithMessage] = page.testAccess
  
  def isRoot_? = page.isRoot_?
      
  def isAbsolute_? = page.isAbsolute_?

  def findLoc(orgPath: ParsePath, path: List[String], req: RequestState, httpReq: HttpServletRequest): Option[Loc] = {
    if (page.doesMatch_?(path, req, httpReq)) Some(page)
    else page.pathMatch(path) match {
      case 0 => first(kids.filter(!_.isAbsolute_?).toList)(_.findLoc(orgPath, orgPath.path, req, httpReq))
      case n =>
      val p2 = path.drop(n)
      first(kids.filter(!_.isAbsolute_?).toList)(_.findLoc(orgPath, p2, req, httpReq)) orElse 
         first(kids.filter(_.isAbsolute_?).toList)(_.findLoc(orgPath, orgPath.path, req, httpReq))
    }
  }
  
  def buildThisLine(loc: Loc) = MenuLine(Nil)
  def buildChildLine = MenuLine(Nil)
  def buildUpperLines: List[MenuLine] = Nil
}

class SiteMapException(msg: String) extends Exception(msg)