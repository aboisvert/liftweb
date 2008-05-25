package webapptest.snippet

import scala.xml.NodeSeq
import net.liftweb.widgets.gravatar.Gravatar

class GravatarDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    val widget = new Gravatar()

    <xml:group>
      {widget.render("tyler.weir@gmail.com")}
    </xml:group>
  }
}
