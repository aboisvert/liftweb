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

case class SiteMap(kids: Menu*) extends HasKids  {
  kids.foreach(_._parent = Some(this))
  kids.foreach(_.init)
  kids.foreach(_.validate)
  
  def findLoc(req: RequestState, httpReq: HttpServletRequest): Option[Loc] = {
    val ret = first(kids.toList)(_.findLoc(req.path, req.path.path, req,httpReq))
    ret
  }
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines: List[MenuLine] = Nil
  def isRoot_? = false
  def buildAboveLine(path: Menu): List[MenuLine] = Nil
  private[sitemap] def testAccess: Option[RedirectWithMessage] = None  
}