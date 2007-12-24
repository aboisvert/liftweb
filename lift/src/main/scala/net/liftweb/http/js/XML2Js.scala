package net.liftweb.http.js

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.xml._
import net.liftweb.util._
import Helpers._


case class Xml2Js(child: NodeSeq) extends Node {
  def label = throw new UnsupportedOperationException("Xml2Js does not have a label")
  
  def toJs = 
    """function(in) {
    var df = document.createDocumentFragment();
    """+
    addToDocFrag("df", child.toList)+
    """
    return df;
    }"""
 
  private def addToDocFrag(parent: String, elems: List[Node]): List[String] = elems.flatMap{
      case Group(nodes) => addToDocFrag(parent, nodes.toList)
      case Text(txt) => List(parent+".appendChild(document.createTextNode("+txt.encJs+"));")
      case Elem(prefix, label, attrs, _, kids :_) =>
        val varName = "v"+randomString(10)
        "var "+varName+" = document.createElement("+label.encJs+");" :: parent+".appendChild("+varName+");" ::
        addToDocFrag(varName, kids.toList)
        
      case _ => Nil
    }
}