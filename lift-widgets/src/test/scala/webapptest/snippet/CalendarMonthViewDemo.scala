package webapptest.snippet


import scala.xml._
import net.liftweb.http._
import net.liftweb.http.S._
import net.liftweb.http.SHtml._
import net.liftweb.http.{RequestVar}
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.util.Can._
import net.liftweb.http.js._
import java.util.Calendar
import java.util.Calendar._

import net.liftweb.widgets.calendar._


class CalendarMonthViewDemo {

  def render(html: Group) : NodeSeq = {
    bind("cal", html,
         "widget" --> CalendarMonthView(makeCals, itemClick, dayClick, weekClick)
    )
  }
  
  import JE._
  import JsCmds._
  
  def itemClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
  def dayClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
  def weekClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))

  
  private def makeCals = {
    val c1 = Calendar getInstance
    val c2 = Calendar getInstance;
    val c3 = Calendar getInstance;

    c2.set(DAY_OF_MONTH, 3)
    c3.set(DAY_OF_MONTH, 1)
    c3.set(MONTH, 4)

    val item1 = CalendarItem("1", c1, Full("Meet me"), Full("We really need to meet to settle things down. This is just a dumb comment to have something in it."), CalendarType.MEETING)
    val item2 = CalendarItem("2", c2, Full("Meet me again"), Empty, CalendarType.MEETING)
    val item3 = CalendarItem("4", c3, Full("Other month"), Empty, CalendarType.MEETING)
    
    item1 :: item2 :: item3 ::  Nil
  }
}
