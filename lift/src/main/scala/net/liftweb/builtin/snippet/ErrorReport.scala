package net.liftweb.builtin.snippet


/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */
   
import net.liftweb.http._
import scala.xml._
import net.liftweb.util.Helpers._

class ErrorReport {
  def render(styles: Group): NodeSeq =
    List((S.errors, (styles \\ "error_msg"), "Error", ((styles \\ "error_class") ++ (styles \\ "error_msg" \\ "@class"))),
        (S.warnings, (styles \\ "warning_msg"), "Warning", ((styles \\ "warning_class")++ (styles \\ "error_msg" \\ "@class"))),
        (S.notices, (styles \\ "notice_msg"), "Notice", ((styles \\ "notice_class")) ++ (styles \\ "notice_msg" \\ "@class"))).flatMap {
    case (msg, titleList, defaultTitle, styleList) =>
      val title: String = titleList.toList.filter(_.prefix == "lift").map(_.text.trim).filter(_.length > 0) headOr defaultTitle
      
      msg.toList.map(e => (<li>{e}</li>) ) match {
        case Nil => Nil
        case msgList => val ret = (<div>{title}:<ul>{msgList}</ul></div>)
        styleList.toList.map(_.text.trim).foldLeft(ret)((xml, style) => xml % new UnprefixedAttribute("class", style, Null))
      }
    }
}
