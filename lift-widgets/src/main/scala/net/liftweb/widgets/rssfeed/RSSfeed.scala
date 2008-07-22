package net.liftweb.widgets.rssfeed

import scala.xml._
import java.net.{URLConnection, URL}
import scala.collection.mutable._

class RSSFeed {
  def render(feedUrl: String): NodeSeq = {
    val feed = getFeed(feedUrl)

    var src = new Queue[Node]()

    src += <li class="rsswidgettitle"><b><a href={ (feed \ "channel" \ "link").text }>{ ( feed \ "channel" \ "title" ).text }</a></b></li>

    for (val c <- feed \\ "item") {
      src += <li class="rsswidgetitem"><a href={(c \\ "link").text}>{(c \\ "title").text}</a></li>
    }

    <div class="rsswidget"><ul>{src}</ul></div>
  }

  def getFeed(feedUrl: String): Elem = {
    val u = new URL(feedUrl)
    val con = u.openConnection

    XML.load(con.getInputStream)
  }

}



