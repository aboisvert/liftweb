package com.hellolift.snippet

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import scala.xml._
import com.hellolift.model.Entry

class BlogUtil {

  def menu = {
    S.request.buildMenu.lines match {
      case Nil => Text("No Navigation Defined.")
      case x :: xs => <ul>{x.items.flatMap(buildANavItem(_))}</ul>
    }
  }

  private def buildANavItem(i: MenuItem) = i match {  
    case MenuItem(text, uri, true, _) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, true) => (<li><a href={uri} id="current">{text}</a></li>)
    case MenuItem(text, uri, _, _) => (<li><a href={uri}>{text}</a></li>)
  }

  def entry = {
    val e = new Entry()
    e.toForm(ignore => e.save) // saves the model object when you submit the form.
  }

}
