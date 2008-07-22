package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
  \*                                                 */

import scala.xml.Node
import net.liftweb.util._

case class XmlResponse(xml: Node) extends ToResponse {
  def docType = Empty
  def code = 200
  def headers = List("Content-Type" -> "text/xml")
  def cookies = Nil
  def out = xml
}


