package net.liftweb.http.js

import net.liftweb.http.js._
import net.liftweb.util.{Can, Full, Empty}

import scala.xml.{Elem, NodeSeq}

trait JSArtifacts {

  def toggle(id: String): JsExp

  def hide(id: String): JsExp
  
  def show(id: String): JsExp
  
  def showAndFocus(id: String): JsExp
  
  def serialize(id: String): JsExp
  
  def setHtml(id: String, xml: NodeSeq): JsCmd
  
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

