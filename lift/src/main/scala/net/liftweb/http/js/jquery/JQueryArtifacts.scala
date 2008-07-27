package net.liftweb.http.js.jquery

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JqJE._


object JQueryArtifacts extends LiftUIArtifacts {

 
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
  
  def setHtml(id: String, xml: NodeSeq): JsCmd = JqJsCmds.SetHtml(id, xml)
  
  def focusOnLoad(xml: Elem): NodeSeq = JqJsCmds.FocusOnLoad(xml)

  
}

object JQueryAjax extends LiftAjax {
  
  def ajax(data: String): String = {
    ajax(data, "script");
  }
  
  def ajax(data: String, dataType: String) : String = {
    ajax("POST", 1000, false, data, dataType);
  }

  def ajax(action: String, timeout: long, cache: Boolean, data: String, dataType: String) : String = {
    "jQuery.ajax( {url: '" +S.encodeURL(S.contextPath+"/"+LiftRules.ajaxPath) +
      "',  type: '" + action + 
      "', timeout: " + timeout +
      ", cache: " + cache +
      ", data: " + data + 
      ", dataType: '" + dataType +
      "'});";
  }

  
}