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

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._

import _root_.scala.xml.{NodeSeq}

class SiteMapException(msg: String) extends Exception(msg)

case class SiteMap(kids: Menu*) extends HasKids  {
  import SiteMap._
  private var locs: Map[LocLookup[LocParams], Loc[LocParams]] = Map.empty

  kids.foreach(_._parent = Full(this))
  kids.foreach(_.init(this))
  kids.foreach(_.validate)
  private[sitemap] def addLoc[T <: LocParams](in: Loc[T]) {
    val lookup: LocLookup[LocParams] = in.lookup.asInstanceOf[LocLookup[LocParams]]
    if (locs.isDefinedAt(lookup))
    throw new SiteMapException("Location "+lookup+" defined twice "+
                               locs(lookup)+" and "+in)
    else locs = locs + (lookup -> in.asInstanceOf[Loc[LocParams]])
  }

  def findLoc(name: String): Can[Loc[NullLocParams]] =
  Can(locs.get(NamedLocLookup(name).asInstanceOf[LocLookup[LocParams]]).
      asInstanceOf[Option[Loc[NullLocParams]]])

  def findLoc[T <: LocParams](key: LocLookup[T]): Can[Loc[T]] =
  Can(locs.get(key.asInstanceOf[LocLookup[LocParams]]).
      asInstanceOf[Option[Loc[T]]])

  def findLoc(req: RequestState): Can[Loc[_]] =
  first(kids)(_.findLoc(req))

  def locForGroup(group: String): Seq[Loc[_]] =
  kids.flatMap(_.locForGroup(group)).filter(_.testAccess match {
      case Left(true) => true case _ => false})
}

object SiteMap {
  def findLoc(name: String): Can[Loc[NullLocParams]] =
  for (sm <- LiftRules.siteMap;
       loc <- sm.findLoc(name)) yield loc

  def findAndTestLoc(name: String): Can[Loc[NullLocParams]] =
  findLoc(name).flatMap(l => l.testAccess match {
      case Left(true) => Full(l)
      case _ => Empty
    })
  
  def findLoc[T <: LocParams](key: LocLookup[T]): Can[Loc[T]] =
  for (sm <- LiftRules.siteMap;
       loc <- sm.findLoc(key)) yield loc

  def findAndTestLoc[T <: LocParams](key: LocLookup[T]): Can[Loc[T]] =
  findLoc(key).flatMap(l => l.testAccess match {
      case Left(true) => Full(l)
      case _ => Empty
    })

  def buildLink(name: String, text: NodeSeq): NodeSeq =
  for (loc <- findAndTestLoc(name).toList;
       param <- loc.defaultParams;
       link <- loc.link.createLink(param))
  yield <a href={link}>{
      text match {
        case x if x.text.length > 0 => x
        case _ => loc.text.text()
      }
    }</a>

  def buildLink(name: String): NodeSeq =
  buildLink(name, Nil)
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem] 
  = populate
  
  def isRoot_? = false

  private[sitemap] def testAccess: Either[Boolean, Can[LiftResponse]] = Left(true)
}
