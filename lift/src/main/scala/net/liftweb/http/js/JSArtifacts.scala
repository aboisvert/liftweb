package net.liftweb.http.js

import net.liftweb.http.js._
import scala.xml.{Elem, NodeSeq}

trait JSArtifacts {

  type JsChain = JsExp with JsMethod
  
  implicit def exp2(exp: JsExp): JsChain = new JsExp with JsMethod {
    def toJsCmd = exp.toJsCmd
  }

  def toggle(id: String): JsChain
  def hide(id: String): JsChain
  def show(id: String): JsChain
  def each(func: String) : JsChain
  def serialize(id: String): JsChain

  def setHtml(id: String, xml: NodeSeq): JsCmd
  def focusOnLoad(xml: Elem): NodeSeq
  def onLoad(cmd: JsCmd): JsCmd
  
  
  def ajax(data: String,  props: (String, String)*): String 
  def ajaxRaw(props: (String, String)*) : String
  
}

