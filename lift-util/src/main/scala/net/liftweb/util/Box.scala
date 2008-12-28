package net.liftweb.util

/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import _root_.scala.reflect.Manifest

/**
 * The Box object provide methods to create Boxes from:<li>
 * <ul/> an Option
 * <ul/> a List
 * <ul/> any AnyRef object
 * </li>
 * It also holds implicit methods to transform: Option to Box, Box to Iterable, Box to Option
 */
object Box {
  /**
   * @returns a Box object from an Option. Full(x) if the Option is Some(x) and Empty otherwise
   */
  def apply[T](in: Option[T]) = in match {
    case Some(x) => Full(x)
    case _ => Empty
  }

  /**
   * This method is used to transform a List with 0 or one element to a Box.
   * @returns a Box object from the head of a List. Full(x) if the List contains at least one element and Empty otherwise.
   */
  def apply[T](in: List[T]) = in match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  def apply[InType, OutType](pf: PartialFunction[InType, OutType])(value: InType): Box[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty

  def apply[InType, OutType](value: InType)(pf: PartialFunction[InType, OutType]): Box[OutType] =
  if (pf.isDefinedAt(value)) Full(pf(value)) else Empty


  /**
   * This method is used to transform any AnyRef to a Box.
   * @returns a Box object from an object. Full(in) if the object is not null and Empty otherwise.
   */
  //def apply[T <: AnyRef](in: T): Box[T] = type2Box(in)

  /**
   * This implicit def allows to use Iterable methods on a Box: size, foreach,...
   * @returns List(in) if the Box is Full(in) and Nil otherwise
   */
  implicit def box2Iterable[T](in: Box[T]): Iterable[T] = in.toList

  /**
   * This implicit def allows to use Options as Boxes
   * @returns a Box object from an Option. Full(in) if the Option is Some(in) and Empty otherwise
   */
  implicit def option2Box[T](in: Option[T]): Box[T] = Box(in)

  /**
   * This implicit def allows to use Boxes as Options
   * @returns Some(in) if the Box is Full(in) and None otherwise
   */
  implicit def box2Option[T](in: Box[T]): Option[T] = in.toOption

  /**
   * This def allows to use any object as a Box, permitting null values to be handled as Empty
   * @returns Full(in) if in is not null Empty otherwise
   */
  def legacyNullTest[T](in: T): Box[T] = in match {
    case null => Empty
    case _ => Full(in)
  }

  /**
   * This def allows to use any object as a Box, permitting null values to be handled as Empty
   * @returns Full(in) if in is not null Empty otherwise
   */
  def !![T](in: T): Box[T] = legacyNullTest(in)

  def isA[A, B](in: A, clz: Class[B]): Box[B] =
  (Box !! in).isA(clz)

  def asA[B](in: T forSome {type T})(implicit m: Manifest[B]): Box[B] =
  (Box !! in).asA[B]

}

/**
 * The Box class is a container which is able to declare if it is Full (with a non-null value) or Empty.
 * It serves the same purpose as the Option class from Scala standard library but adds several features:<li>
 * <ul> you can transform it to a Failure object if it is Empty (with the ?~ method)
 * <ul> you can chain failure messages on Failure Boxes
 * <ul> you "run" a function on your Box, with a default value: <code>Full(1).run("zero") { x: String, y: Int => y.toString }</code>
 * <ul> you can "pass" a Box to a funtion for side effects: <code>Full(1) $ { x: Box[Int] => println(x openOr 0) }</code>
 * </li>
 */
@serializable
sealed abstract class Box[+A] extends Product {
  /**
   * @returns true if the Box contains no value
   */
  def isEmpty: Boolean

  /**
   * @returns false if the Box contains a value
   */
  def isDefined: Boolean = !isEmpty

  /**
   * @returns the value of the Box if it is full. Throw an exception otherwise
   */
  def open_! : A

  /**
   * @returns the value of the Box if it is full. Returns a default value otherwise
   */
  def openOr[B >: A](default: => B): B = default

  /**
   * applies a function on the Box's value if it exists
   * @returns the modified Box or an Empty Box
   */
  def map[B](f: A => B): Box[B] = Empty

  /**
   * applies a function returning a Box on the Box's value if it exists and removes the "inner" Box if necessary
   * @returns the modified Box or an Empty Box
   */
  def flatMap[B](f: A => Box[B]): Box[B] = Empty

  /**
   * @returns this Box if it has a value satisfying a predicate
   */
  def filter(p: A => Boolean): Box[A] = this

  /**
   * @returns true if the Box's value verifies a predicate
   */
  def exists(func: A => Boolean): Boolean = false

  /**
   * applies a function to the Box value
   */
  def foreach(f: A => Any): Unit = {}

  /**
   * If the contents of the Box isA instance of
   * the given class, return a Full[B], otherwise
   * Empty
   */
  def isA[B](cls: Class[B]): Box[B] = Empty

  /**
   * If the contents of the Box are an instance of the given
   * type, return a Full[B], otherwise Empty
   */
  def asA[B](implicit m: Manifest[B]): Box[B] = Empty

  /**
   * @returns a this or an alternative Box if this is an Empty Box
   */
  def or[B >: A](alternative: => Box[B]): Box[B] = alternative

  /**
   * @returns an iterator on the Box value
   */
  def elements: Iterator[A] = Iterator.empty

  /**
   * @returns true if the Box's value verifies a predicate
   */
  def toList: List[A] = Nil

  /**
   * @returns the Box
   */
  def toOption: Option[A] = None

  /**
   * @param msg the failure message
   * @returns a Failure with the message if the Box is an Empty Box
   */
  def ?~(msg: String): Box[A] = this

  /**
   * Alias for ?~
   */
  def failMsg(msg: String): Box[A] = ?~(msg)

  /**
   * @param msg the failure message
   * @returns a Failure with the message if the Box is an Empty Box. Chain the messages if it is already a Failure
   */
  def ?~!(msg: String): Box[A] = ?~(msg)

  /**
   * Alias for ?~!
   */
  def compoundFailMsg(msg: String): Box[A] = ?~!(msg)

  /**
   * @param msg the failure message
   * @param p a predicate
   * @returns a Failure with the message if the predicate is not satisfied with the Box's value
   */
  def filterMsg(msg: String)(p: A => Boolean): Box[A] = filter(p) ?~ msg

  /**
   * runs a function on the Box's value
   * @returns the result of the function or a default value
   */
  def run[T](in: T)(f: (T, A) => T) = in

  /**
   * pass the Box's value to a function
   * @returns the Box
   */
  def pass(f: Box[A] => Unit): Box[A] = {f(this) ; this}

  /**
   * Alias for pass
   */
  def $(f: Box[A] => Unit): Box[A] = pass(f)

  /**
   * overrides the equals method for Boxes (For Full and Empty only. For Failure, the method is overriden again)
   */
  override def equals(other: Any): Boolean = (this, other) match {
    case (Full(x), Full(y)) => x == y
    case (Full(x), y) => x == y
    case (x, y: AnyRef) => x eq y
    case _ => false
  }

  /**
   * applies the function f1 if possible, return an alternative Box otherwise
   */
  def choice[B](f1: A => Box[B])(alternative: => Box[B]): Box[B] = this match {
    case Full(x) => f1(x)
    case _ => alternative
  }

  def ===[B >: A](to: B): Boolean = false

  /**
   * Run the map or return the default value
   */
  def dmap[B](dflt: => B)(f: A => B): B = dflt
}

/**
 * The Full Box is a Box containing a value.
 * It provides adequate behavior to a Box for when a value is involved
 */
@serializable
final case class Full[+A](value: A) extends Box[A] {

  def isEmpty: Boolean = false

  def open_! : A = value

  override def openOr[B >: A](default: => B): B = value

  override def or[B >: A](alternative: => Box[B]): Box[B] = this

  override def exists(func: A => Boolean): Boolean = func(value)

  override def filter(p: A => Boolean): Box[A] = if (p(value)) this else Empty

  override def foreach(f: A => Any): Unit = f(value)

  override def map[B](f: A => B): Box[B] = Full(f(value))

  override def flatMap[B](f: A => Box[B]): Box[B] = f(value)

  override def elements: Iterator[A] = Iterator.fromValues(value)

  override def toList: List[A] = List(value)

  override def toOption: Option[A] = Some(value)

  override def run[T](in: T)(f: (T, A) => T) = f(in, value)

  /**
   * If the contents of the Box isA instance of
   * the given class, return a Full[B], otherwise
   * Empty
   */
  override def isA[B](cls: Class[B]): Box[B] = value match {
    case value: AnyRef =>
      if (cls.isAssignableFrom(value.getClass)) Full(value.asInstanceOf[B])
      else Empty
    case _ => Empty
  }

  /**
   * If the contents of the Box are an instance of the given
   * type, return a Full[B], otherwise Empty
   */
  override def asA[B](implicit m: Manifest[B]): Box[B] = this.isA(m.erasure).asInstanceOf[Box[B]]

  override def ===[B >: A](to: B): Boolean = value == to

  /**
   * Run the map or return the default value
   */
  override def dmap[B](dflt: => B)(f: A => B): B = f(value)
}

/**
 * Singleton object representing an Empty Box
 */
@serializable
case object Empty extends EmptyBox[Nothing]

/**
 * The EmptyBox is a Box containing no value.
 * It provides adequate behavior to a Box for when no value is involved
 */
@serializable
sealed abstract class EmptyBox[+A] extends Box[A] {

  def isEmpty: Boolean = true

  def open_!  = throw new NullPointerException("Trying to open an empty Box")

  override def openOr[B >: A](default: => B): B = default

  override def or[B >: A](alternative: => Box[B]): Box[B] = alternative

  override def filter(p: A => Boolean): Box[A] = this

  override def ?~(msg: String) = Failure(msg, Empty, Empty)
}

object Failure {
  def apply(msg: String) = new Failure(msg, Empty, Empty)
}

/**
 * A Failure is an Empty Box having a failure message explaining the reason for being empty
 * It can also optionally provide an exception or a chain of causes represented as a list of other Failure objects
 */
@serializable
sealed case class Failure(msg: String, exception: Box[Throwable], chain: Box[Failure]) extends EmptyBox[Nothing] {
  type A = Nothing

  override def ?~(msg: String) = this

  override def ?~!(msg: String) = Failure(msg, Empty, Full(this))

  override def map[B](f: A => B): Box[B] = this

  override def flatMap[B](f: A => Box[B]): Box[B] = this

  /**
   * If the contents of the Box isA instance of
   * the given class, return a Full[B], otherwise
   * Empty
   */
  override def isA[B](cls: Class[B]): Box[B] = this

  /**
   * If the contents of the Box are an instance of the given
   * type, return a Full[B], otherwise Empty
   */
  override def asA[B](implicit m: Manifest[B]): Box[B] = this

  private def chainList: List[Failure] = chain match {
    case Full(f) => f :: f.chainList
    case _ => Nil
  }

  def messageChain: String = (this :: chainList).map(_.msg).mkString(" <- ")

  override def equals(other: Any): Boolean = (this, other) match {
    case (Failure(x, y, z), Failure(x1, y1, z1)) => (x, y, z) == (x1, y1, z1)
    case (x, y: AnyRef) => x eq y
    case _ => false
  }
}

/**
 * A ParamFailure extends Failure, but has a parameter that can
 * allow an application to pass other information as a response
 */
@serializable
final case class ParamFailure[T](override val msg: String,
				 override val exception: Box[Throwable],
				 override val chain: Box[Failure], param: T) extends
Failure(msg, exception, chain)

// vim: set ts=2 sw=2 et:
