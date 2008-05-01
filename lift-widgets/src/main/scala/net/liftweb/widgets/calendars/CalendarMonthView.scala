package net.liftweb.widgets.calendar;

import scala.xml._
import java.util.Calendar
import java.util.Calendar._
import java.text.SimpleDateFormat
import net.liftweb.util.Helpers._
import net.liftweb.util.Can
import net.liftweb.http.js._
import net.liftweb.http.SHtml._
import JsCmds._
import JE._

object CalendarMonthView {

  /**
   * Call this function typically in boot
   */
  def init() {
    import net.liftweb.http.ResourceServer
    ResourceServer.allow({
      case "calendars" :: tail => true
    })
  }

  def apply(calendars: Seq[CalendarItem], 
            itemClick: Can[AnonFunc], 
            dayClick: Can[AnonFunc], 
            weekClick: Can[AnonFunc]) = new CalendarMonthView().render(calendars, itemClick, dayClick, weekClick)
}

/**
 * CalendarMonthView renders a month view representation of a collection of CalendarItem
 * <br> 
 * Usage example - assume CalendarView is a typical LiftWeb snippet
 * <pre>
 * class CalendarView {
 * 
 *  def render(html: Group) : NodeSeq = {
 *    bind("cal", html,
 *         "widget" --> CalendarMonthView(makeCals, itemClick, dayClick, weekClick)
 *    )
 *  }
 *  
 *  import JE._
 *  import JsCmds._
 *  
 *  def itemClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 *  def dayClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 *  def weekClick = Full(AnonFunc("elem, param", JsRaw("alert(param + ' - ' + elem.nodeName)")))
 * 
 *  
 *  private def makeCals = {
 *    val c1 = Calendar getInstance
 *    val c2 = Calendar getInstance;
 *    val c3 = Calendar getInstance;
 * 
 *    c2.set(DAY_OF_MONTH, 3)
 *    c3.set(DAY_OF_MONTH, 1)
 *    c3.set(MONTH, 4)
 * 
 *    val item1 = CalendarItem("1", c1, Full("Meet me"), Full("We really need to meet to settle things down"), CalendarType.MEETING)
 *    val item2 = CalendarItem("2", c2, Full("Meet me again"), Empty, CalendarType.MEETING)
 *    val item3 = CalendarItem("4", c3, Full("Other month"), Empty, CalendarType.MEETING)
 *    
 *    item1 :: item2 :: item3 ::  Nil
 *  }
 * }
 * 
 * </pre>
 *
 */
class CalendarMonthView {
  private val weekDaysFormatter = new SimpleDateFormat("EEEE")
  private val timeFormatter = new SimpleDateFormat("hh:mm")
  private val dateFormatter = new SimpleDateFormat("MM/dd/yyyy")
  var meta: CalendarMeta = new CalendarMeta
  
  /**
   * Returns the markup for rendering the calendar month view
   *
   * @param calendars - the calendar items than need to be rendered
   * @param itemClick - Ajax function to be called when a calendar item was clicked. 
   *                    It takes two parameters: elem the node that was clicked and 
   *                    param the identifier if this CalendarItem
   * @param dayClick - Ajax function to be called when a day number(cell header) item was clicked
   *                   It takes two parameters: elem the node that was clicked and 
   *                   param the date of the clicked day in MM/dd/yyyy format
   * @param weekClick - Ajax function to be called when a day number(cell header) item was clicked
   *                   It takes two parameters: elem the node that was clicked and 
   *                   the week number
   * @return NodeSeq - the markup to be rendered  
   */
  def render(calendars: Seq[CalendarItem], 
             itemClick: Can[AnonFunc], 
             dayClick: Can[AnonFunc],
             weekClick: Can[AnonFunc]): NodeSeq = {
     
    def makeCells(calendar: Calendar, thisMonth: int): NodeSeq = {
      val cal = calendar.clone().asInstanceOf[Calendar] 
      val today = Calendar getInstance (meta locale)
      (0 to 5) map (row => <tr><td wk={cal get(WEEK_OF_YEAR) toString} 
                                   class={meta.cellWeek} 
                                   onclick={JsFunc("weekClick", JsRaw("this"), Jq(JsRaw("this")) >> JqGetAttr("wk")).toJsCmd}>
        {cal get(WEEK_OF_YEAR)}</td>{(0 to 6) map (col => 
        try{
         <td>{
            val day = cal.get(DAY_OF_MONTH)
            val month = cal.get(MONTH)
            val isThisMonth = thisMonth == month
            val isToday = today.get(DAY_OF_MONTH) == cal.get(DAY_OF_MONTH) && (month == thisMonth)
            val div = <div>{
              calendars filter (c => day == c.when.get(DAY_OF_MONTH) && month == c.when.get(MONTH)) map (c => {
                val r = <div><a href="#">{
                   <span>{timeFormatter format(c.when.getTime)} {c.subject openOr "..."}</span> 
                }</a></div> % ("class" -> meta.calendarItem) % 
                  ("rec_id" -> c.id) % 
                  ("onclick" -> JsFunc("itemClick", JsRaw("this"), Jq(JsRaw("this")) >> JqGetAttr("rec_id")).toJsCmd)
                  
                c.description map (desc => r % (("title" -> desc))) openOr r
              }
              )
            }</div>
            val (head, cell) = isToday match {
              case true => (meta.cellHeadToday, meta.cellBodyToday)
              case _ => (month != thisMonth) match {
                case true => (meta.cellHeadOtherMonth, meta.cellBodyOtherMonth)
                case _ => (meta.cellHead, meta.cellBody)
              }
            }
            Group(<div>{day}</div> % 
              ("class" -> head) :: 
              div % ("class" -> cell) :: Nil)
          }</td> % ("date" -> (dateFormatter format(cal getTime))) %
            ("onclick" -> JsFunc("dayClick", JsRaw("this"), (Jq(JsRaw("this")) >> JqGetAttr("date"))).toJsCmd)
        } finally {
          cal add(DAY_OF_MONTH, 1)
        }
        ) 
      }</tr>)
    }
    
    def makeHead(headCal: Calendar) = <tr><td></td>{
      (0 to 6) map(x => <td width="14%">{
        try{
          weekDaysFormatter format(headCal getTime)
        } finally {
          headCal add(DAY_OF_MONTH, 1)
        }
      }</td>)
    }</tr>
    
    val cal: Calendar = Calendar getInstance(meta locale)
    cal set(DAY_OF_MONTH, 1)
    val thisMonth = cal get(MONTH)
    val delta = cal.get(DAY_OF_WEEK) - meta.firstDayOfWeek
    cal add(DAY_OF_MONTH, if (delta < 0) -delta-7 else -delta)
    
    val headCal = cal.clone().asInstanceOf[Calendar]
    
    val init = JsRaw("""
      jQuery(function($){
        jQuery('.calendarItem').click(function(e){
          e = e || window.event
          if (e.stopPropagation) { 
            e.stopPropagation()
          }
          e.cancelBubble = true;
        });
        jQuery('.calendarItem').tooltip({ 
          track: true, 
          delay: 0, 
          showURL: false,
          extraClass: 'box'
        });
      })
      """) & 
      JsCrVar("itemClick", itemClick openOr JsRaw("function(param){}")) &
      JsCrVar("dayClick", dayClick openOr JsRaw("function(param){}")) & 
      JsCrVar("weekClick", weekClick openOr JsRaw("function(param){}"))
    
      <head>
        <link rel="stylesheet" href="/classpath/calendars/monthview/style.css" type="text/css"/>
        <script type="text/javascript" src="/classpath/calendars/monthview/jquery.dimensions.js"></script>
        <script type="text/javascript" src="/classpath/calendars/monthview/jquery.bgiframe.js"></script>
        <script type="text/javascript" src="/classpath/calendars/monthview/jquery.tooltip.js"></script>
        <script type="text/javascript" charset="utf-8">{Unparsed(init toJsCmd)}</script>
      </head>
      <div class={meta.monthView}>{
        <table width="100%" cellspacing="1" cellpadding="0" style="table-layout: fixed;" class={meta.topHead}>
          {makeHead(headCal)}
          {makeCells(cal, thisMonth)}
        </table> 
      }</div>
  }
}

