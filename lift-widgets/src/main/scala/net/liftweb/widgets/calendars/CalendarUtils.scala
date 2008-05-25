package net.liftweb.widgets.calendar;

import net.liftweb.http.js._
import net.liftweb.util._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmds._
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Calendar._


object CalendarUtils {
  private lazy val timeFormatter = new SimpleDateFormat("h:m")

  import JE._
  import JsCmds._

  /**
   * Returns the JSON representation of the list of CalendrItem sorted by start time.
   */
  def toJSON(items: List[CalendarItem]): JsExp = {

    JsObj(("items", JsArray(items.sort((e1, e2) => e1.start before(e2 start)) map(c => {
      
      val (sh, sm) = (c.start.get(HOUR_OF_DAY), c.start.get(MINUTE));
      val (eh, em) = c.end map (c => (c.get(HOUR_OF_DAY), c.get(MINUTE))) openOr (48, 0)

      val startIndex = sm match {
        case x if sm >= 30 => sh*2 + 1
        case _ => sh*2
      }
      
      val endIndex = em match {
        case x if em > 30 => eh*2 + 2
        case x if em > 0 => eh*2 + 1
        case _ => eh*2
      } 

      var items: List[(String, JsExp)] = ("id", Str(c.id)) ::
        ("start", JsRaw(startIndex toString)) :: 
        ("end", c.end map(c => JsRaw(endIndex toString)) openOr JsRaw("48")) ::
        ("weekDay", Str(weekDay(c start) toString)) ::
        ("startTime", Str(timeFormatter.format(c.start getTime))) :: 
        ("subject", Str(c.subject openOr "")) :: Nil
        
      items = c.description map(desc => items ++ (("description", Str(desc)) :: Nil) ) openOr items
      
      JsObj(items:_*)
     
      
    }):_*))) 
  }

  def weekDay(cal: Calendar) = {
    cal get (DAY_OF_WEEK)
  }
}
