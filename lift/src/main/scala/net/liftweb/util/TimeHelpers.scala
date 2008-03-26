package net.liftweb.util
import java.text.SimpleDateFormat
import java.util.{TimeZone, Calendar, Date}

/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers
/**
 * The TimeHelpers trait provide functions to create TimeSpans (an object representing an amount of time), to manage date formats
 * or general utility functions (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers extends ControlHelpers {
  
  /** private variable allowing the access to all TimeHelpers functions from inside the TimeSpan class */
  private val outer = this 

  /** transforms a long to a TimeSpan object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def longToTimeSpan(in: long): TimeSpan = TimeSpan(in)
  /** transforms an int to a TimeSpan object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def intToTimeSpan(in: int): TimeSpan = TimeSpan(in)

  /** 
   * transforms a TimeSpan to a date by converting the TimeSpan expressed as millis and creating
   * a Date lasting that number of millis from the Epoch time (see the documentation for java.util.Date)  
   */
  implicit def timeSpanToDate(in: TimeSpan): java.util.Date = new Date(in.len)

  /** 
   * The TimeSpan class represents an amount of time. The exact unit for that amount of time is 
   *   
   */
  class TimeSpan(val len: Long) {
    def seconds = TimeSpan(outer.seconds(len))
    def second = seconds
    def minutes = TimeSpan(outer.minutes(len))
    def minute = minutes
    def hours = TimeSpan(outer.hours(len))
    def hour = hours
    def days = TimeSpan(outer.days(len))
    def day = days
    def weeks = TimeSpan(outer.weeks(len))
    def week = weeks
    def later = TimeSpan(len + millis)
    def ago = TimeSpan(millis - len)
    def date = new Date(len)
    def +(in: long) = TimeSpan(this.len + in)
    def -(in: long) = TimeSpan(this.len - in)

    def noTime = {
      val div = (12L * 60L * 60L * 1000L)
      val ret = (len - div) / (div * 2L)
      new Date((ret * (div * 2L)) + div)
    }
    override def equals(cmp: Any) = {
      cmp match {
        case lo: long => lo == this.len
        case i: int => i == this.len
        case ti: TimeSpan => ti.len == this.len
        case _ => false
      }
    }
    override def toString = {
      def moof(in: long, scales: List[(long, String)]): List[long] = {
        val hd = scales.head
        val sc = hd._1
        if (sc >= 10000L) List(in)
        else (in % sc) :: moof(in / sc, scales.tail)
      }
      
      val lst = moof(len, TimeSpan.scales).zip(TimeSpan.scales.map(_._2)).reverse.dropWhile(_._1 == 0L).filter(_._1 > 0).map(t => ""+t._1+" "+t._2+(if (t._1 != 1L) "s" else ""))
        lst.mkString("",", ", "")
    }
  }
  object TimeSpan {
    val scales = List((1000L, "milli"), (60L, "second"), (60L, "minute"), (24L, "hour"), (7L, "day"), (1000000000L, "week"))
    def apply(in: long) = new TimeSpan(in)
    implicit def timeSpanToLong(in: TimeSpan): long = in.len
    def format(in: Long): String = {
      scales.foldLeft[(Long, List[(Long, String)])]((in, Nil)){(total, div) =>
        (total._1 / div._1, (total._1 % div._1, div._2) :: total._2) 
      }._2.filter(_._1 > 0).map{case (amt, measure) if amt == 1 => amt+" "+measure
        case (amt, measure) => amt+" "+measure+"s"
      }.mkString(" ")
    }
  }
  def millis = System.currentTimeMillis
  def seconds(in: long): long = in * 1000L
  def minutes(in: long): long = seconds(in) * 60L
  def hours(in: long): long = minutes(in) * 60L
  def days(in: long): long = hours( in) * 24L
  def weeks(in: long): long = days(in) * 7L

  val utc = TimeZone.getTimeZone("UTC")
  def internetDateFormatter = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss z")
    ret.setTimeZone(utc)
    ret
  }
  
  def parseInternetDate(dateString: String): Date = tryo {
    internetDateFormatter.parse(dateString)
  } openOr new Date(0L)
  
  def toInternetDate(in: Date): String = internetDateFormatter.format(in)
  def toInternetDate(in: long): String = internetDateFormatter.format(new Date(in))
  
  def dateFormatter = new SimpleDateFormat("yyyy/MM/dd")
  def timeFormatter = new SimpleDateFormat("HH:mm zzz")
  
  def formattedTimeNow = timeFormatter.format(timeNow)
  def formattedDateNow = dateFormatter.format(timeNow)
  def currentYear: Int = Calendar.getInstance.get(Calendar.YEAR)
  
  def logTime[T](msg: String)(f: => T): T = {
    val (time, ret) = calcTime(f)
    Log.info(msg+" took "+time+" Milliseconds")
    ret
  }
  
  def calcTime[T](f: => T): (Long, T) = {
    val start = millis
    val ret = f
    (millis - start, ret)
  }
  
  /**
   * Given the input date, what's the month (0 based)?
   */
  def month(in: Date): Int = {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.MONTH)
  }
  
  /**
   * Given the input date, what's the year?
   */
  def year(in: Date): Int =  {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.YEAR)
  }
  
  /**
   * Given the input date, what's the day (1 based)?
   */
  def day(in: Date): Int =  {
    val cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
    cal.setTimeInMillis(in.getTime)
    cal.get(Calendar.DAY_OF_MONTH)
  }
  
  /**
   * The current time as a Date object
   */
  def timeNow = new Date
  
  /**
   * Convert the incoming millis to days since epoch
   */
  def millisToDays(millis: Long): Long = millis / (1000L * 60L * 60L * 24L)
  
  /**
   * The number of days since epoch
   */
  def daysSinceEpoch: Long = millisToDays(millis)
  
  /**
   * The current Day as a Date object
   */
  def dayNow: Date = TimeSpan(0).seconds.later.noTime
  def time(when: long) = new Date(when)
  
  val hourFormat = new SimpleDateFormat("HH:mm:ss")
  
  def hourFormat(in: Date): String = {
    hourFormat.format(in)
  }    

  def toDate(in: Any): Can[Date] = {
    try {
      in match {
        case null => Empty
        case d: java.util.Date => Full(d)
        case lng: Long => Full(new Date(lng))
        case lng: Number => Full(new Date(lng.longValue))
        case Nil | Empty | None | Failure(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s : String => Full(new Date(s))
        case o => toDate(o.toString)
      }
    } catch {
      case e => Log.debug("Error parsing date "+in, e); Failure("Bad date: "+in, Full(e), Nil)
    }
  }
}
