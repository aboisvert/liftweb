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
  class JsEscaper(val in: String) {
    def esc: String = {
      val len = in.length
      val sb = new StringBuilder(len * 2)
      sb.append('\'')
      var pos = 0
      while (pos < len) {
        in.charAt(pos) match {
          case c @ ('\\' | '\'') => sb.append(escChar(c))
          case c if c < ' ' || c > '~' => sb.append(escChar(c))
          case c => sb.append(c) 
        }
        pos += 1
      }
      sb.append('\'')
      sb.toString
    }

    
    def escChar(in: Char): String = {
      val ret = Integer.toString(in.toInt, 16)
      "\\u"+("0000".substring(ret.length))+ret
    }
  }
  
  implicit def strToJsEsc(in: String): JsEscaper = new JsEscaper(in)
  
  case class CmdPair(left: JsCmd, right: JsCmd) extends JsCmd {
    def toJsCmd = left.toJsCmd + "\n" + right.toJsCmd
  }
  case class Set(uid: String, content: NodeSeq) extends JsCmd {
    def toJsCmd = {
      val html = S.request.fixHtml(content).toString 
      val ret = "try{jQuery("+("#"+uid).esc+").each(function(i) {this.innerHTML = "+html.esc+";});} catch (e) {}"
      ret
    }
  }

  trait HasTime {
    def time: Can[Int]
    
    def timeStr = time.map(_.toString) openOr ""
  }

  object Show {
    def apply(uid: String) = new Show(uid, Empty)
    def apply(uid: String, time: Int) = new Show(uid, Full(time))
  }
  
  class Show(val uid: String,val time: Can[Int]) extends JsCmd with HasTime {
    def toJsCmd = "try{jQuery("+("#"+uid).esc+").show("+timeStr+");} catch (e) {}"
  }

  object Hide {
    def apply(uid: String) = new Hide(uid, Empty)
    def apply(uid: String, time: Int) = new Hide(uid, Full(time))    
  }
  
  class Hide(val uid: String,val time: Can[Int]) extends JsCmd with HasTime {
     def toJsCmd = "try{jQuery("+("#"+uid).esc+").hide("+timeStr+");} catch (e) {}"
  }

  case class Alert(text: String) extends JsCmd {
    def toJsCmd = "alert("+text.esc+");"
  }
  
  case class ModalDialog(html: NodeSeq) extends JsCmd {
    def toJsCmd = "jQuery.blockUI("+AltXML.toXML(Group(S.session.map{s => s.fixHtml(s.processSurroundAndInclude(html))}.openOr(html)), false).esc+");"
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
}
