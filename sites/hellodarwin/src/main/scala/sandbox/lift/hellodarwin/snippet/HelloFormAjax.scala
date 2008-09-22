package sandbox.lift.hellodarwin.snippet

import scala.xml.NodeSeq
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.{JsCmd, JsCmds, JE}

class HelloFormAjax {
  def whoNode(str: String) = <span id="who">{str}</span>

  def updateWho(str: String): JsCmd = {
    JsCmds.SetHtml("who", whoNode(str))
  }

  def show(xhtml: NodeSeq): NodeSeq = {
    <xml:group>
      Hello {whoNode("world")}
      <br/>
      <label for="whoField">Who :</label>
      { text("world", null) % ("size" -> "10") % ("id" -> "whoField") }
      { <button type="button">{?("Send")}</button> % 
       ("onclick" -> 
	ajaxCall(JE.JsRaw("$('#whoField').attr('value')"),
		 s => updateWho(s))) }
    </xml:group>
  }
}

