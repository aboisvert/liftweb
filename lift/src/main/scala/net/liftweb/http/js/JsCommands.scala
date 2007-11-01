package net.liftweb.http.js

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.xml.{NodeSeq, Group}
import net.liftweb.util.Helpers._
import net.liftweb.util._

object JsCommands {
  def create = new JsCommands(Nil)  
  def apply(in: Seq[JsCmd]) = new JsCommands(in.toList.reverse)
}

class JsCommands(val reverseList: List[JsCmd]) extends ResponseIt {
  def +(in: JsCmd) = new JsCommands(in :: reverseList)
  def +(in: List[JsCmd]) = new JsCommands(in.reverse ::: reverseList)
  
  def toResponse = {
    val data = reverseList.reverse.map(_.toJsCmd).mkString("\n").getBytes("UTF-8")
    Response(data, List("Content-Length" -> data.length.toString, "Content-Type" -> "text/javascript"), 200)
  }
}

abstract class JsCmd {
  def +(other: JsCmd): JsCmd = JsCmds.CmdPair(this, other)  
  def toJsCmd: String
}

object JsCmds {
  case class CmdPair(left: JsCmd, right: JsCmd) extends JsCmd {
    def toJsCmd = left.toJsCmd + "\n" + right.toJsCmd
  }
  case class Set(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = {
      val html = S.request.fixHtml(content).toString 
      val ret = "try{jQuery("+("#"+uid).encJs+").each(function(i) {this.innerHTML = "+html.encJs+";});} catch (e) {}"
      ret
    }
  }

  trait HasTime {
    def time: Can[Long]
    
    def timeStr = time.map(_.toString) openOr ""
  }
  
  case class After(millis: Long, toDo: JsCmd) extends JsCmd {
    def toJsCmd = "setTimeout(function() {"+toDo.toJsCmd+"}, "+millis+");"
  }

  object Show {
    def apply(uid: String) = new Show(uid, Empty)
    def apply(uid: String, time: Long) = new Show(uid, Full(time))
  }
  
  class Show(val uid: String,val time: Can[Long]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).encJs+").show("+timeStr+");} catch (e) {}"
  }

  object Hide {
    def apply(uid: String) = new Hide(uid, Empty)
    def apply(uid: String, time: Long) = new Hide(uid, Full(time))    
  }
  
  class Hide(val uid: String,val time: Can[Long]) extends JsCmd with HasTime {
     def toJsCmd = "try{jQuery("+("#"+uid).encJs+").hide("+timeStr+");} catch (e) {}"
  }

  case class Alert(text: String) extends JsCmd {
    def toJsCmd = "alert("+text.encJs+");"
  }
  
  case class ModalDialog(html: NodeSeq) extends JsCmd {
    def toJsCmd = "jQuery.blockUI("+AltXML.toXML(Group(S.session.map{s => s.fixHtml(s.processSurroundAndInclude(html))}.openOr(html)), false).encJs+");"
  }

  case class Run(text: String) extends JsCmd {
    def toJsCmd = text
  }

  case object Noop extends JsCmd {
    def toJsCmd = ""
  }
  
  case object Unblock extends JsCmd {
    def toJsCmd = "jQuery.unblockUI();"
  }
  
  case class JsTry(what: JsCmd) extends JsCmd {
    def toJsCmd = "try { "+what.toJsCmd+" } catch (e) {alert(e);}"
  }
  
  case class RedirectTo(where: String) extends JsCmd {
    private val context = S.contextPath
    def toJsCmd = "window.location = "+(context + where).encJs+";"
  }
  
  case class DisplayMessage(where: String, msg: NodeSeq, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def toJsCmd = (Show(where) + Set(where, msg) + After(duration, Hide(where, fadeTime))).toJsCmd
  }
}
