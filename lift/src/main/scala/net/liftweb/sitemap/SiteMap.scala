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

case class SiteMap(kids: Menu*) extends HasKids  {
  kids.foreach(_._parent = Full(this))
  kids.foreach(_.init)
  kids.foreach(_.validate)

  def findLoc(req: RequestState): Can[Loc] = {
    val ret = first(kids.toList)(_.findLoc(req))
    ret
  }
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines: List[MenuLine] = Nil
  def isRoot_? = false
  def buildAboveLine(path: Menu): List[MenuLine] = Nil
  private[sitemap] def testAccess: (Boolean, Can[ConvertableResponse]) = (true, Empty)
}
