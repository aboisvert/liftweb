/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package net.liftweb.http.js.jquery

import scala.xml.{Elem, NodeSeq}

import net.liftweb.http.S
import net.liftweb.http.js.JE
import net.liftweb.http.js.JsCmds
import JE._
import JqJE._


object JQueryArtifacts extends JSArtifacts {

 
  def toggle(id: String) = JqId(id) ~> new JsMethod {
    def toJsCmd = "toggle()" 
  }
  
  def hide(id: String) = JqId(id) ~> new JsMethod {
    def toJsCmd = "hide()" 
  }
  
  def show(id: String) = JqId(id) ~> new JsMethod {
    def toJsCmd = "show()" 
  }
  
  def showAndFocus(id: String) = JqId(id) ~> new JsMethod {
    def toJsCmd = "show().each(function(i) {var t = this; setTimeout(function() { t.focus(); }, 200);})" 
  } 

  def serialize(id: String) = JqId(id) ~> new JsMethod {
    def toJsCmd = "serialize()" 
  }
  
  def setHtml(id: String, xml: NodeSeq): JsCmd = JqJsCmds.SetHtml(id, xml)
  
  def onLoad(cmd: JsCmd): JsCmd = JqJsCmds.OnLoad(cmd)
  
  def ajax(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, LiftRules.ajaxPath) + ");"
  }
  
  def comet(data: AjaxInfo): String = {
    "jQuery.ajax(" + toJson(data, LiftRules.cometPath) + ");"
  }
  
  def jsonStringify(in: JsExp) : JsExp = new JsExp {
    def toJsCmd = "JSON.stringify(" + in.toJsCmd + ")"
  }

  def formToJSON(formId: String):JsExp = new JsExp() {
    def toJsCmd = "lift$.formToJSON('" + formId + "')";
  }

  private def toJson(info: AjaxInfo, path: String): String = ("url : '" + S.encodeURL(S.contextPath+"/"+path) + "'" ::
    "data : " + info.data ::
    "type : '" + info.action + "'" ::
    "dataType : '" + info.dataType + "'"  ::
    "timeout : " + info.timeout ::
    "cache : " + info.cache :: Nil) ++ 
    info.successFunc.map("success : " + _).toList ++ 
    info.failFunc.map("error : " + _).toList mkString("{ ", ", ", " }")
}
