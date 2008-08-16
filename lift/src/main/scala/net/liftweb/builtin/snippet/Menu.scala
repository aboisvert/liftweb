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
        case x :: xs =>
          val map = S.prefixedAttrsToMap("li")
          val md = S.mapToAttrs(map)

          <ul>{x.items.flatMap(buildANavItem(md, map))}</ul> %
          S.prefixedAttrsToMetaData("ul")
        
      }).openOr(Text("No Navigation Defined."))
  }

  private def buildANavItem(li: MetaData, liMap: Map[String, String])(i: MenuItem) = i match {
    case MenuItem(text, uri, true, _, _) => 
      (<li><span>{text}</span></li>) % S.prefixedAttrsToMetaData("li_item", liMap)
    case MenuItem(text, uri, _, true, _) => 
      (<li><a href={uri}>{text}</a></li>) % S.prefixedAttrsToMetaData("li_path", liMap)
    case MenuItem(text, uri, _, _, _) => (<li><a href={uri}>{text}</a></li> % li)
  }
  
  def item(text: NodeSeq): NodeSeq = 
  for (name <- S.attr("name").toList;
       request <- S.request.toList;
       loc <- request.location.toList if loc.name != name;
       item <- SiteMap.buildLink(name, text))
  yield item match {
    case e: Elem => e % S.prefixedAttrsToMetaData("a")
    case x => x
  }
}
