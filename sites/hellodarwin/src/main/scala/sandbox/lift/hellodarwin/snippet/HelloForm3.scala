package sandbox.lift.hellodarwin.snippet

import scala.xml.NodeSeq
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers._
import net.liftweb.util.Full

class HelloForm3 {
  object who extends RequestVar(Full("world"))

  def show(xhtml: NodeSeq): NodeSeq = {
    <xml:group>
      Hello {who.openOr("")}
      <br/>
      <label for="whoField">Who :</label>
      { text(who.openOr(""), v => who(Full(v))) % ("size" -> "10") % ("id" -> "whoField") }
      { submit(?("Send"), println("value:" + who.openOr(""))) }
    </xml:group>
  }
}

