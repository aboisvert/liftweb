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
package net.liftweb.builtin.snippet

import net.liftweb.http.S
import net.liftweb.sitemap._
import scala.xml._

class Menu {
  def builder: NodeSeq = {
    S.request.map(_.buildMenu.lines match {
      case Nil => Text("No Navigation Defined.")
      case x :: xs => <ul>{x.items.flatMap(buildANavItem(_))}</ul>
    }).openOr(Text("No Navigation Defined."))
  }

  private def buildANavItem(i: MenuItem) = i match {
    case MenuItem(text, uri, true, _, _) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, true, _) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, _, _) => (<li><a href={uri}>{text}</a></li>)
  }
}
