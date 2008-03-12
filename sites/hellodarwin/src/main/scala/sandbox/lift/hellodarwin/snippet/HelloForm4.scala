package sandbox.lift.hellodarwin.snippet

import scala.xml.NodeSeq
import net.liftweb.http.S._
import net.liftweb.http.StatefulSnippet
import net.liftweb.util.Helpers._
import net.liftweb.util.Full

class HelloForm4 extends StatefulSnippet{

  val dispatch: DispatchIt = {
    case "show" => show _
  }

  var who = "world"

  def show(xhtml: NodeSeq): NodeSeq = {
    <xml:group>
      Hello {who}
      <br/>
      <label for="whoField">Who :</label>
      { text(who, v => who = v) % ("size" -> "10") % ("id" -> "whoField") }
      { submit(?("Send"), ignore => {println("value:" + who)}) }
    </xml:group>
  }
}

