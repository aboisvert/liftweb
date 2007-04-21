package net.liftweb.sitemap

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import net.liftweb.http._
import javax.servlet.http.{HttpSessionActivationListener, HttpSessionEvent, HttpServletRequest}

case class SiteMap(map: Menu*) {
  def findLoc(req: RequestState, httpReq: HttpServletRequest): Option[Loc] = None
}
