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
  case class SetHtml(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = {
      val html = AltXML.toXML(Group(S.session.map(s => s.fixHtml(s.processSurroundAndInclude(content))).openOr(content)), false, true) 
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
    def toJsCmd = "jQuery.blockUI("+AltXML.toXML(Group(S.session.map(s => s.fixHtml(s.processSurroundAndInclude(html))).openOr(html)), false, true).encJs+");"
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
  
  case class JsTry(what: JsCmd, alert: Boolean) extends JsCmd {
    def toJsCmd = "try { "+what.toJsCmd+" } catch (e) {"+(if (alert) "alert(e);" else "")+"}"
  }
  
  case class SetValueAndFocus(id: String, value: String) extends JsCmd {
    def toJsCmd = "jQuery('#"+id+"').attr('value', "+value.encJs+"); document.getElementById("+id.encJs+").focus();"
  }
  
  case class RedirectTo(where: String) extends JsCmd {
    private val context = S.contextPath
    def toJsCmd = "window.location = "+(context + where).encJs+";"
  }


  /**
    * Update a Select with new Options
    */
  case class ReplaceOptions(select: String, opts: List[(String, String)], dflt: Can[String]) extends JsCmd {
    def toJsCmd = """var x=document.getElementById("""+select.encJs+""");
    while (x.length > 0) {x.remove(0);}
    var y = null;
    """+
      opts.map{case (value, text) => "y=document.createElement('option'); "+
      "y.text = "+text.encJs+"; "+
      "y.value = "+value.encJs+"; "+
      (if (value == dflt) "y.selected = true; " else "") + "x.add(y, null); "
      }.mkString("\n")
  }
  
  case class DisplayMessage(where: String, msg: NodeSeq, duration: TimeSpan, fadeTime: TimeSpan) extends JsCmd {
    def realFadeTime: Long = fadeTime.toLong match {
      case x if x <= 0 => 0
      case x if x < 100 => x * 1000L
      case x => x
    }
    
    def realDuration: Long = duration.toLong match {
      case x if x <= 0 => 10.seconds.toLong
      case x if x < 100 => x * 1000L
      case x => x
    }
    
    def toJsCmd = (Show(where) + SetHtml(where, msg) + After(realDuration, Hide(where, realFadeTime))).toJsCmd
  }
}
