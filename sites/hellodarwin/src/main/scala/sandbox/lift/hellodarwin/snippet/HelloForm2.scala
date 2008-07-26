package sandbox.lift.hellodarwin.snippet

import scala.xml.NodeSeq
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers._
import net.liftweb.util.Full

class HelloForm2 {
  object who extends RequestVar(Full("world"))

  def show(xhtml: NodeSeq): NodeSeq = {
    bind("hello", xhtml,
        "whoField" -> text(who.openOr(""), v => who(Full(v))) % ("size" -> "10") % ("id" -> "whoField"),
        "submit" -> submit(?("Send"), println("value:" + who.openOr("") + " :: " + param("whoField"))),
        "who" -> who.openOr("")
    )
  }
}

