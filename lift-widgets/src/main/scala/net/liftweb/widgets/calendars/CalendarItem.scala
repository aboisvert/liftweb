package net.liftweb.widgets.calendar;

import java.util.Calendar
import java.util.Calendar._
import net.liftweb.util.{Can, Empty, Full}

object CalendarItem {
  def apply(id: String, start: Calendar, calendarType: CalendarType.Value) = new CalendarItem(id, start, calendarType)
}
case class CalendarItem(id: String, 
                        start: Calendar, 
                        calendarType: CalendarType.Value,
                        end: Can[Calendar],
                        subject: Can[String], 
                        description: Can[String]) {

  def this(id: String, start: Calendar, calendarType: CalendarType.Value) = this(id, start, calendarType, Empty, Empty, Empty)
  
  def end (end: Calendar) = CalendarItem(id, start, calendarType, Can.legacyNullTest(end), Empty, Empty)
  def subject (subject: String) = CalendarItem(id, start, calendarType, Empty, Can.legacyNullTest(subject), Empty)
  def description(description: String) = CalendarItem(id, start, calendarType, Empty, Empty, Can.legacyNullTest(description))
  
  private def choose[T](l: Can[T], r: Can[T]): Can[T] = l match {case Empty => r case _ => l} 
  
  def optional(f: (CalendarItem) => CalendarItem*): CalendarItem = {
    f.map(c => c(this)).foldLeft(this)((l, r) => CalendarItem(id, start, calendarType,
        choose(l end, r end),
        choose(l subject, r subject),
        choose(l description, r description)),
    )
  }
}

object CalendarType extends Enumeration {
  val MEETING, ANIVERSARY, TODO, EVENT = Value
}




