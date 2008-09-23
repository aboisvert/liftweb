package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.gravatar.Gravatar

class GravatarDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    val widget = new Gravatar()

    <xml:group>
      {widget.render("tyler.weir@gmail.com")}
    </xml:group>
  }
}
