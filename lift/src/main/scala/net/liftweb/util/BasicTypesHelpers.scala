package net.liftweb.util
import _root_.java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import _root_.scala.xml._

/**
 * This object adds functionalities to Scala standard types
 */
object BasicTypesHelpers extends BasicTypesHelpers with StringHelpers with ControlHelpers

/**
 * This trait adds functionalities to Scala standard types
 */
trait BasicTypesHelpers { self: StringHelpers with ControlHelpers =>

  /**
   * transforms a Boolean to a Boolean2, allowing expressions such as:
   * <code>(1 == 2) ? "a" | "b"</code> (This expression will return "b")
   */
  implicit def boolean2(b: Boolean) = new Boolean2(b)

  /**
   * This class adds a ternary operator to Boolean expressions
   */
  class Boolean2(b: Boolean) {
    /* ternary operator for the Boolean b */
    def ? [A](first: A): BooleanOption[A] = {
      if (b) BooleanSome(first)
      else BooleanNone
    }
    /* This case class has a | operator returning a default value depending if the Option is Some or None */
    sealed abstract class BooleanOption[+A] {
      def |[B >: A](default: => B): B
    }
    case class BooleanSome[+A](x: A) extends BooleanOption[A] {
      def |[B >: A](default: => B): B  = x
    }
    case object BooleanNone extends BooleanOption[Nothing] {
      def |[B](default: => B): B  = default
    }
  }

  /**
   * Optional cons that implements the expression: expr ?> value ::: List
   */
  class OptionalCons(expr: Boolean) {
    def ?>[T](f: => T): List[T] = if (expr) List(f) else Nil
  }

  /**
   * transforms a Boolean expression in in an OptionalCons object so that an element can
   * be added to a list if the expression is true
   */
  implicit def toOptiCons(expr: Boolean): OptionalCons = new OptionalCons(expr)

  /**
   * Convert any object to an "equivalent" Boolean depending on its value
   */
  def toBoolean(in: Any): Boolean = {
    in match {
      case null => false
      case b : Boolean => b
      case i: Int => i != 0
      case lo: Long => lo != 0
      case n : Number => n.intValue != 0
      case s : String => {
        val sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else if (sl == "yes") true
          else toInt(s) != 0
        }
      }
      case None => false
      case Empty | Failure(_, _, _) => false
      case Full(n) => toBoolean(n)
      case Some(n) => toBoolean(n)
      case x :: xs => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }

  def asInt(in: String): Can[Int] = tryo{in.toInt}

def asLong(in: String): Can[Long] = tryo(in.toLong)

  /**
   * Convert any object to an "equivalent" Int depending on its value
   */
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: Int => n
      case lo: Long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case Full(n) => toInt(n)
      case None | Empty | Failure(_, _, _) => 0
      case s: String => parseNumber(s).toInt
      case d: _root_.java.util.Date => (d.getTime / 1000L).toInt
      case x :: xs => toInt(x)
      case o => toInt(o.toString)
    }
  }

  /**
   * Convert any object to an "equivalent" Long depending on its value
   */
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: Int => i
      case n: Long => n
      case d: _root_.java.util.Date => d.getTime
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case Full(n) => toLong(n)
      case None | Empty | Failure(_, _, _) => 0L
      case s: String => parseNumber(s)
      case x :: xs => toLong(x)
      case o => toLong(o.toString)
    }
  }

  /**
   * Convert any InputStream to a ByteArrayInputStream
   */
  def toByteArrayInputStream(in: InputStream) = {
    val ba = new Array[Byte](4096)
    val bos = new ByteArrayOutputStream
    var len = 0
    while (len >= 0) {
      len = in.read(ba)
      if (len > 0) {
        bos.write(ba, 0, len)
      }
    }
    new ByteArrayInputStream(bos.toByteArray)
  }

  /**
   * @return true if two Byte arrays don't contain the same bytes
   */
  def notEq(a: Array[Byte], b: Array[Byte]) = !isEq(a,b)

  /**
   * @return true if two Byte arrays contain the same bytes
   */
  def isEq(a: Array[Byte], b: Array[Byte]) = {
    def eq(a: Array[Byte], b: Array[Byte], pos: Int, len: Int): Boolean = {
      if (pos == len) true
      else if (a(pos) != b(pos)) false
      else eq(a , b, pos + 1, len)
    }
    a.length == b.length && eq(a, b, 0, a.length)
  }
}
