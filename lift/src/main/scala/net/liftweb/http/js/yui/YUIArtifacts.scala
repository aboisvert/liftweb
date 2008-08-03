package net.liftweb.http.js.yui

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import net.liftweb.util.Helpers
import Helpers._
import JsCmds._
import JE._

/**
 * Prerequisite YUI scripts:
 * yahoo.js
 * dom.js
 * connection.js
 * event.js
 */
object YUIArtifacts extends JSArtifacts {
  
  def toggle(id: String) = new JsExp {
    def toJsCmd = "YAHOO.lift.toggle(this, '" + id + "');"; 
  }
  
  def hide(id: String) = new JsExp {
    def toJsCmd = "YAHOO.util.Dom.setStyle('" + id + "', 'display', 'none');" 
  }
  
  def show(id: String) = new JsExp {
    def toJsCmd = "YAHOO.util.Dom.setStyle('" + id + "', 'display', 'block');" 
  }
  
  def showAndFocus(id: String) = new JsExp {
    def toJsCmd = "YAHOO.util.Dom.setStyle('" + id + "', 'display', 'block');" + 
                  "setTimeout(function() { document.getElementById('" + id + "').focus(); }, 200);" 
  }
  
  def serialize(id: String) = new JsExp {
    def toJsCmd = "YAHOO.util.Connect.setForm('" + id+"', false);" 
  }
  
  def setHtml(uid: String, content: NodeSeq): JsCmd = new JsCmd {
    def toJsCmd = "try{document.getElementById('" + uid + "').innerHTML = " + fixHtml(uid, content)+";} catch (e) {}"
  }
  
  def onLoad(cmd: JsCmd): JsCmd = new JsCmd {
    def toJsCmd = "YAHOO.util.Event.onDOMReady(function(){" + cmd.toJsCmd + "})"
  }
  
  def ajax(data: AjaxInfo): String = {
    "YAHOO.util.Connect.asyncRequest('POST', " + 
      S.encodeURL(S.contextPath+"/"+LiftRules.ajaxPath) + ", " +
      toJson(data) + ", " +    
      data.data +
      ")"
  }
  
  def comet(data: AjaxInfo): String = {
    "YAHOO.util.Connect.asyncRequest('POST', " + 
      S.encodeURL(S.contextPath+"/"+LiftRules.cometPath) + ", " +
      toJson(data) + ", " +    
      data.data +
      ")"
  }

  private def toJson(info: AjaxInfo): String = 
    ("timeout : " + info.timeout ::
     "cache : " + info.cache ::  
     "success : function(resp) { if (resp.getResponseHeader('Content-Type') == 'text/javascript') {YAHOO.lift.eval(resp);}" + 
       info.successFunc.map(_ + "(resp)").openOr("") + "}" ::
     "failure : " + info.failFunc.openOr ("function (arg) {YAHOO.log('Ajax request failed');}") :: 
     Nil) mkString("{ ", ", ", " }")


}
