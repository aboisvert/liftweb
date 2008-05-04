/*
* Copyright 2007-2008 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions
* and limitations under the License.
*/
  
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
        choose(l description, r description))
    )
  }
}

object CalendarType extends Enumeration {
  val MEETING, ANIVERSARY, EVENT, ALLDAY = Value
}




