package net.liftweb.http.js

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.xml._
import net.liftweb.util._
import Helpers._

/*
case class Jx(child: NodeSeq) extends Node {
  def label = throw new UnsupportedOperationException("Xml2Js does not have a label")
  
  def toJs = 
    """function(in) {
    var df = document.createDocumentFragment();
    """+
    addToDocFrag("df", child.toList)+
    """
    return df;
    }"""
 
  private def addAttrs(varName: String, attrs: List[MetaData]): List[String] = attrs.flatMap {
      case up: UnprefixedAttribute => List(varName+".setAttribute("+up.key.encJs+","+up.value.text.encJs+");")
      case pe: PrefixedAttribute if pe.pre == "l" =>  List(varName+".setAttribute("+pe.key.encJs+","+pe.value.text+");")
      case _ => Nil
    }
      
  private def addToDocFrag(parent: String, elems: List[Node]): List[String] = elems.flatMap{
      case Group(nodes) => addToDocFrag(parent, nodes.toList)
      case Text(txt) => List(parent+".appendChild(document.createTextNode("+txt.encJs+"));")
      case Elem(prefix, label, attrs :_ , _, kids :_) =>
        val varName = "v"+randomString(10)
        "var "+varName+" = document.createElement("+label.encJs+");" :: parent+".appendChild("+varName+");" ::
        addAttrs(varName, attrs.toList) :::
        addToDocFrag(varName, kids.toList)
      case Xml2Js(kids) => addToDocFrag(parent, kids.toList)
      case _ => Nil
    }
}*/