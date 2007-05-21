package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.{Node}
import scala.collection.immutable.Map
import net.liftweb.util._

case class XhtmlResponse(out : Node, headers : Map[String, String], code : int) {
  def toResponse = Response(AltXML.toXML(out, false).getBytes("UTF-8"), headers, code)
}
case class Response(data: Array[byte], headers : Map[String, String], code : int)
