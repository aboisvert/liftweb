package net.liftweb.util

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

object Maybe {
  /** An implicit conversion that converts an option to an iterable value
   */
  implicit def option2Iterable[a](xo: Maybe[a]): Iterable[a] = xo.toList
}
    

/** This class represents optional values. Instances of <code>Maybe</code>
 *  are either instances of case class <code>Success</code>, case class <code>Failure</code> or it is case
 *  object <code>Not</code>.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.1, 16/01/2007
 */
sealed abstract class Maybe[+A] extends Product {

  /** True if the option is the <code>Success</code> value, false otherwise.
   */
  def isEmpty: Boolean

  /** True if the option is a <code>Success</code>(...) false otherwise.
   */
  def isDefined: Boolean = !isEmpty

  /** get the value of this option.
   *  @requires that the option is nonEmpty.
   *  @throws Predef.NoSuchElementException if the option is empty.
   */
  def get: A

  /** If the option is nonempty return its value,
   *  otherwise return the result of evaluating a default expression.
   *
   *  @param default  the default expression.
   */
  def getOrElse[B >: A](default: => B): B = 
    if (isEmpty) default else this.get

  /** If the option is nonempty, return a function applied to its value,
   *  wrapped in a Success i.e. <code>Success(f(this.get))</code>.
   *  Otherwise return <code>None</code>.
   *
   *  @param  f   the function to apply
   */
  def map[B](f: A => B): Maybe[B] = if (!isEmpty) Success(f(get)) else this match {
     case f : Failure => f 
     case Success(x) => Success(f(x))
   }

  /** If the option is nonempty, return a function applied to its value.
   *  Otherwise return None.
   *  @param  f   the function to apply
   */
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = if (!isEmpty) f(get) else this match {
  case f : Failure => f
  case Success(x) => f(x)
}

  /** If the option is nonempty and the given predicate <code>p</code>
   *  yields <code>false</code> on its value, return <code>None</code>.
   *  Otherwise return the option value itself.
   *
   *  @param  p   the predicate used for testing.
   */
  def filter(p: A => Boolean): Maybe[A] = if (isEmpty || p(this.get)) this else Failed

  /** Apply the given procedure <code>f</code> to the option's value,
   *  if it is nonempty. Do nothing if it is empty.
   *
   *  @param  f   the procedure to apply.
   */
  def foreach(f: A => Unit) {
    if (!isEmpty) f(this.get)
  }

  /** If the option is nonempty return it,
   *  otherwise return the result of evaluating an alternative expression.
   *  @param alternative  the alternative expression.
   */
  def orElse[B >: A](alternative: => Maybe[B]): Maybe[B] = 
    if (isEmpty) alternative else this

  /** An singleton iterator returning the option's value if it is nonempty
   *  or the empty iterator if the option is empty.
   */
  def elements: Iterator[A] = 
    if (isEmpty) Iterator.empty else Iterator.fromValues(this.get)

  /** A singleton list containing the option's value if it is nonempty
   *  or the empty list if the option is empty.
   */
  def toList: List[A] = 
    if (isEmpty) List() else List(this.get)
    
  def $(msg: String): Maybe[A] = if (isEmpty) Failure(msg) else this
  
  def fail(msg: String): Maybe[A] = if (isEmpty) Failure(msg) else this
}

/** Class <code>Success[A]</code> represents existing values of type
 *  <code>A</code>.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
final case class Success[+A](x: A) extends Maybe[A] {
  def isEmpty = false
  def get = x
}

class Failure(val reason: String) extends Maybe[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException("Nothing.get")
  def productArity = 1
  def productElement(what: Int): Any = reason
}

object Failure {
  def apply(reason: String) = new Failure(reason)
  def unapply(in: Failure): Option[String] = Some(in.reason)
}

/** This case object represents non-existent values.
 */
case object Failed extends Failure("empty")

object DoesItWork {
  def test(i1: Maybe[Int], i2: Maybe[String]): Maybe[String] = for (x1 <- i1 fail "No i1"; x2 <- i2 fail "No i2") yield (x2.length - x1).toString
    
  def foo(in: Maybe[Int]): String = in match {
    case Failure(reason) => reason
    case Failed => "D'oh!"
    case Success(n) => n.toString
  }
}

