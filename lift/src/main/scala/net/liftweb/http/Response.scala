package net.liftweb.http

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.NodeSeq
import scala.collection.immutable.Map

case class Response(out : NodeSeq, headers : Map[String, String], code : int)
