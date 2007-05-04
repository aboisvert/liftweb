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

case class SiteMap(map: Menu*) {
  map.foreach(_.init)
  map.foreach(_.validate)
  
  def findLoc(req: RequestState, httpReq: HttpServletRequest): Option[Loc] = {
    first(map.toList)(_.findLoc(req.path, req.path.path, req,httpReq))
  }
}
