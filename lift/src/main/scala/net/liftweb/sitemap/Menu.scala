package net.liftweb.sitemap

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

case class Menu(page: Loc, kids: Menu*) {
  private var _parent: Option[Menu] = None
  kids.foreach(_._parent = Some(this))
}
