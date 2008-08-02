package net.liftweb.http.js

import net.liftweb.http.js._
import net.liftweb.util.{Can, Full, Empty}

import scala.xml.{Elem, NodeSeq}

trait JSArtifacts {

  type JsChain = JsExp with JsMethod
  
  implicit def exp2(exp: JsExp): JsChain = new JsExp with JsMethod {
    def toJsCmd = exp.toJsCmd
  }

  /**
   * Used to toggle a xhtml node
   */
  def toggle(id: String): JsChain
  def hide(id: String): JsChain
  def show(id: String): JsChain
  def each(func: String) : JsChain
  def serialize(id: String): JsChain

  def setHtml(id: String, xml: NodeSeq): JsCmd
  def focusOnLoad(xml: Elem): NodeSeq
  def onLoad(cmd: JsCmd): JsCmd
  
  
  def ajax(data: AjaxInfo): String 
  def comet(data: AjaxInfo): String

}

object AjaxInfo {
  def apply(data:String) = new AjaxInfo(data, "POST", 1000, false, "script", Empty, Empty)
  def apply(data:String, dataType: String) = new AjaxInfo(data, "POST", 1000, false, dataType, Empty, Empty)
  
  def apply(data:String, 
            timeout: Long, 
            successFunc: String, 
            failFunc: String) = new AjaxInfo(data, 
                                             "POST", 
                                             timeout, 
                                             false, 
                                             "script", 
                                             Full(successFunc), 
                                             Full(failFunc))
}

case class AjaxInfo(data: String, action: String, timeout: Long, cache: boolean, dataType: String, successFunc: Can[String], failFunc: Can[String]) 

