package net.liftweb.http

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb._
import util._
import Helpers._
import scala.xml.{NodeSeq, Elem}
  
trait StatefulSnippet {
  def dispatch(name: String): Can[NodeSeq => NodeSeq]
  
  private[http] var snippetName: String = ""
    
  def registerThisSnippet = S.setSnippetForClass(snippetName, this);
  
  /**
     * create an anchor tag around a body 
     *
     * @param func - the function to invoke when the link is clicked
     * @param body - the NodeSeq to wrap in the anchor tag
     */
   def link(to: String, func: () => Any, body: NodeSeq): Elem = S.link(to, () => {registerThisSnippet; func()}, body) 
}