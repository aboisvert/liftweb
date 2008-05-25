package webapptest.snippet

import scala.xml.NodeSeq
import net.liftweb.widgets.rssfeed.RSSFeed

class RSSFeedDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    val widget = new RSSFeed()

    <xml:group>
      {widget.render("http://www.praytothemachine.com/evil/index.php/feed/")}
    </xml:group>
  }
}
