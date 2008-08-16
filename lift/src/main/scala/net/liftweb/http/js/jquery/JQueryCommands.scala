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

import scala.xml.{NodeSeq, Group, Unparsed, Elem, Node, SpecialNode}
import net.liftweb.util.Helpers._
import net.liftweb.util.Helpers
import net.liftweb.util.TimeHelpers
import net.liftweb.util._

import net.liftweb.http.js.{JsExp, JE}
import JE._
import JsCmds._


trait JQueryRight {
  this: JsExp =>
  def toJsCmd: String
}

trait JQueryLeft {
  this: JsExp =>
  def >>(that: JQueryRight): JsExp = new JsExp {
    def toJsCmd = JQueryLeft.this.toJsCmd + "."+ that.toJsCmd
  }


  def >>(that: JQueryLeft with JQueryRight): JsExp with JQueryLeft =
    new JsExp with JQueryLeft {
      def toJsCmd = JQueryLeft.this.toJsCmd + "."+ that.toJsCmd
  }
}

object JqJE {
    case object JqTabsSelected extends JsExp with JQueryRight {
    def toJsCmd = "tabsSelected()"
  }

  case object JqScrollToBottom extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "each(function(i) {this.scrollTop=this.scrollHeight;})"
  }

  object JqTabsClick {
    def apply(tab: JsExp): JsExp with JQueryRight with JQueryLeft =
    new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "tabsClick("+tab.toJsCmd+")"
    }

    def apply(tab: Int): JsExp with JQueryRight with JQueryLeft =
    apply(Num(tab))
  }

  object JqTabs {
    def apply(in: JsExp): JsExp with JQueryRight with JQueryLeft =
    new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "tabs("+in.toJsCmd+")"
    }

    def apply(): JsExp with JQueryRight with JQueryLeft =
    apply(JsRaw(""))
  }

  case class JqClick(exp: JsExp) extends JsExp with JQueryLeft with JQueryRight {
    def toJsCmd = "click(" + exp.toJsCmd + ")"
  }

  case class JqGetAttr(key: String) extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "attr(" + key.encJs + ")"
  }

    /**
   * A JQuery query
   */
  case class Jq(query: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery("+query.toJsCmd+")"
  }

  /**
   * A JQuery query for an element based on the id of the element
   */
  case class JqId(id: JsExp) extends JsExp with JQueryLeft {
    override def toJsCmd = "jQuery('#'+"+id.toJsCmd+")"
  }

  case class JqAttr(key: String, value: JsExp) extends JsExp with JQueryRight with JQueryLeft {
    def toJsCmd = "attr("+key.encJs+", "+value.toJsCmd+")"
  }

  /**
   * Append content to a JQuery
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "append("+fixHtml("inline", content)+")"
  }

  /**
   * AppendTo content to a JQuery
   */
  case class JqAppendTo(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "appendTo("+fixHtml("inline", content)+")"
  }

  /**
   * Prepend content to a JQuery
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "prepend("+fixHtml("inline", content)+")"
  }

  /**
   * PrependTo content to a JQuery
   */
  case class JqPrependTo(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "prependTo("+fixHtml("inline", content)+")"
  }

  /**
   * EmptyAfter will empty the node at the given uid and stick the given content behind it. Like
   * a cleaner innerHTML.
   */
  case class JqEmptyAfter(content: NodeSeq) extends JsExp with JQueryRight with JQueryLeft {
    override def toJsCmd = "empty().after("+fixHtml("inline", content)+")"
  }

  object JqHtml {
    def apply() = new JsExp with JQueryRight {
      def toJsCmd = "html()"
    }

    def apply(content: NodeSeq) = new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "html("+fixHtml("inline", content)+")"
    }
  }

  object JqText {
    def apply() = new JsExp with JQueryRight {
      def toJsCmd = "text()"
    }

    def apply(content: String) = new JsExp with JQueryRight with JQueryLeft {
      def toJsCmd = "text("+content.encJs+")"
    }
  }

  /**
   * Serialize input elements intoa string data. ALso works for serializing forms
   */
  case object JqSerialize extends JsExp with JQueryRight {
    def toJsCmd = "serialize()"
  }

  /**
   * Serialize the jquery into a JSON array
   */
  case object JsonSerialize extends JsExp with JQueryRight {
    def toJsCmd = "serializeArray()"
  }

}

object JqJsCmds {
  
  implicit def jsExpToJsCmd(in: JsExp) = in.cmd

  case class OnLoad(cmd: JsCmd) extends JsCmd {
    def toJsCmd = "jQuery(document).ready(function() {"+cmd.toJsCmd+"});"
  }
  
  /**
   * Append a NodeSeq to a node specified by uid using jQuery's append() method.
   */
  object AppendHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) >> JqJE.JqAppend(content)
  }

  /**
   * AppendTo a NodeSeq to a node specified by uid using jQuery's appendTo() method.
   */
  object AppendToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) >> JqJE.JqAppendTo(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery's prepend() method.
   */
  object PrependHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) >> JqJE.JqPrepend(content)
  }

  /**
   * Replaces the children of the node at {@code uid} with {@code content}
   */
  object EmptyAfter {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) >> JqJE.JqEmptyAfter(content)
  }

  /**
   * Prepends a NodeSeq to a node specified by uid using jQuery prependTo() method.
   */
  object PrependToHtml {
    def apply(uid: String, content: NodeSeq): JsCmd =
      JqJE.JqId(JE.Str(uid)) >> JqJE.JqPrependTo(content)
  }


  case class JqSetHtml(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = {
      val ret = "try{jQuery("+("#"+uid).encJs+").each(function(i) {this.innerHTML = "+fixHtml(uid, content)+";});} catch (e) {}"
      ret
    }
  }
  
  object Show {
    def apply(uid: String) = new Show(uid, Empty)
    def apply(uid: String, time: TimeSpan) = new Show(uid, Full(time))
  }

  class Show(val uid: String,val time: Can[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).encJs+").show("+timeStr+");} catch (e) {}"
  }

  object Hide {
    def apply(uid: String) = new Hide(uid, Empty)
    def apply(uid: String, time: TimeSpan) = new Hide(uid, Full(time))
  }

  class Hide(val uid: String, val time: Can[TimeSpan]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).encJs+").hide("+timeStr+");} catch (e) {}"
  }

  case class DisplayMessage(where: String, msg: NodeSeq, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (Show(where) & JqSetHtml(where, msg) & After(duration, Hide(where, fadeTime))).toJsCmd
  }

  object ModalDialog {
    def apply(html: NodeSeq) = new ModalDialog(html, Empty)
    def apply(html: NodeSeq, width: String) = new ModalDialog(html, Full(width))
  }

  class ModalDialog(html: NodeSeq, width: Can[String]) extends JsCmd {
    def toJsCmd = "jQuery.blockUI("+AltXML.toXML(Group(S.session.map(s =>
    s.fixHtml(s.processSurroundAndInclude("Modal Dialog", html))).openOr(html)), false, true).encJs+
    (width.map(w => ", { width: '"+w+"' }").openOr("")) + ");"
  }

  case object Unblock extends JsCmd {
    def toJsCmd = "jQuery.unblockUI();"
  }

  case class SetValueAndFocus(id: String, value: String) extends JsCmd {
    def toJsCmd = "jQuery('#"+id+"').attr('value', "+value.encJs+"); document.getElementById("+id.encJs+").focus();"
  }

}
