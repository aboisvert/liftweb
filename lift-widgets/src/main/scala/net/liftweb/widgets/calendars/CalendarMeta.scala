package net.liftweb.widgets.calendar;

import java.util.Locale
import java.util.Calendar._

class CalendarMeta {
  val cellHeadOtherMonth = "cellHeadOtherMonth"
  val cellBodyOtherMonth = "cellBodyOtherMonth"
  val cellHead = "cellHead"
  val cellBody = "cellBody"
  val cellWeek = "cellWeek"
  val monthView = "monthView"
  val topHead = "topHead"
  val calendarItem = "calendarItem"
  val cellHeadToday = "cellHeadToday"
  val cellBodyToday = "cellBodyToday"
  var firstDayOfWeek = MONDAY
  var locale = Locale getDefault
}
