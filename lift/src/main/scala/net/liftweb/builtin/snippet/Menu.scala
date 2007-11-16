package net.liftweb.builtin.snippet

import net.liftweb.http.S
import net.liftweb.sitemap._
import scala.xml._

class Menu {
  def builder: NodeSeq = {
    S.request.map(_.buildMenu.lines match {
      case Nil => Text("No Navigation Defined.")
      case x :: xs => <ul>{x.items.flatMap(buildANavItem(_))}</ul>
    }).openOr(Text("No Navigation Defined."))
  }

  private def buildANavItem(i: MenuItem) = i match {
    case MenuItem(text, uri, true, _) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, true) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, _) => (<li><a href={uri}>{text}</a></li>)
  }
}
