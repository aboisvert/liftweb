package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import scala.xml.Node
import net.liftweb.util.Can

/**
  * Constructs controllers
  */
trait ControllerFactory {
  def construct(contType: String): Can[ControllerActor]
}
