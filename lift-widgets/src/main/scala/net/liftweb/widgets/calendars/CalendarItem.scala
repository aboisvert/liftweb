package net.liftweb.widgets.calendar;

import java.util.Calendar
import net.liftweb.util.Can

case class CalendarItem(id: String, 
                        when: Calendar, 
                        subject: Can[String], 
                        description: Can[String], 
                        calendarType: CalendarType.Value)

object CalendarType extends Enumeration {
  val MEETING, ANIVERSARY, TODO, EVENT = Value
}


