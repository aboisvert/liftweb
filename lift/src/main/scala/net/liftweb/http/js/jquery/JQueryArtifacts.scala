package net.liftweb.http.js.jquery

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JqJE._


object JQueryArtifacts extends JSArtifacts {

 
  def toggle(id: String): JsChain = JqId(id) ~> new JsMethod {
    def toJsCmd = "toggle()" 
  }
  
  def hide(id: String): JsChain = JqId(id) ~> new JsMethod {
    def toJsCmd = "hide()" 
  }
  
  def show(id: String): JsChain = JqId(id) ~> new JsMethod {
    def toJsCmd = "show()" 
  }

  def each(func: String): JsChain = new JsExp {
    def toJsCmd = "each(" + func + ")" 
  }
  
  def serialize(id: String): JsChain = JqId(id) ~> new JsMethod {
    def toJsCmd = "serialize()" 
  }
  
  def setHtml(id: String, xml: NodeSeq): JsCmd = JqJsCmds.SetHtml(id, xml)
  
  def focusOnLoad(xml: Elem): NodeSeq = JqJsCmds.FocusOnLoad(xml)
  
  def onLoad(cmd: JsCmd): JsCmd = JqJsCmds.OnLoad(cmd)
  
  def ajax(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, LiftRules.ajaxPath) + ");"
  } 
  
  def comet(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, LiftRules.cometPath) + ");"
  }

  
  def toJson(info: AjaxInfo, path: String): String = ("url : '" + S.encodeURL(S.contextPath+"/"+path) + "'" ::
    "data : " + info.data ::
    "type : '" + info.action + "'" ::
    "dataType : '" + info.dataType + "'"  ::
    "timeout : " + info.timeout ::
    "cache : " + info.cache :: Nil) ++ 
    info.successFunc.map(f => "success : " + f).toList ++ 
    info.failFunc.map(f => "error : " + f).toList mkString("{ ", ", ", " }")
}
