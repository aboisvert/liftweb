package net.liftweb.http.js

import net.liftweb.http.js._
import scala.xml.{Elem, NodeSeq}

trait LiftUIArtifacts {

  type JsChain = JsExp with JsMethod
  
  implicit def exp2(exp: JsExp): JsChain = new JsExp with JsMethod {
    def toJsCmd = exp.toJsCmd
  }

  def toggle(id: String): JsChain
  def hide(id: String): JsChain
  def show(id: String): JsChain
  def each(func: String) : JsChain

  def setHtml(id: String, xml: NodeSeq): JsCmd
  def focusOnLoad(xml: Elem): NodeSeq
}

trait LiftAjax {

  def ajax(data: String): String;
  def ajax(data: String, dataType: String): String;
  def ajax(action: String, timeout: long, cache: Boolean, data: String, dataType: String) : String
}