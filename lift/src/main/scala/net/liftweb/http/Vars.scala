package net.liftweb.http

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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

import _root_.net.liftweb.util._
import _root_.scala.collection.mutable.{HashMap, ListBuffer}

/**
 * Abstract a request or a session scoped variable.
 */
abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) {
  self: MyType =>
  private lazy val name = "_lift_sv_"+getClass.getName+"_"+__nameSalt
  protected def findFunc(name: String): Box[T]
  protected def setFunc(name: String, value: T): Unit
  protected def clearFunc(name: String): Unit

  protected def __nameSalt = ""

  /**
   * The current value of the variable
   */
  def is: T = findFunc(name) match {
    case Full(v) => v
    case _ => val ret = dflt
      apply(ret)
      cleanupFunc.foreach(registerCleanupFunc)
      ret
  }

  /**
  * Shadow of the 'is' method
  */
  def get: T = is

  /**
  * Shadow of the apply method
  */
  def set(what: T): Unit = apply(what)

  /**
   * Set the session variable
   *
   * @param what -- the value to set the session variable to
   */
  def apply(what: T): Unit = setFunc(name, what)

  def remove(): Unit = clearFunc(name)

  def cleanupFunc: Box[() => Unit] = Empty

  def registerCleanupFunc(in: () => Unit): Unit

  override def toString = is.toString
}

/**
 * Keep session information around without the nastiness of naming session variables
 * or the type-unsafety of casting the results.
 * SessionVars are type-safe variables that map pretty directly to
 * HttpSession attributes.  Put stuff in and they are available for the
 * life of the Session.
 *
 * SessionVar's can be used even from CometActor's as now S scope in a Cometctor is
 * provided automatically.
 *
 * @param dflt - the default value of the session variable
 */
abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) {
  override protected def findFunc(name: String): Box[T] = S.session.flatMap(_.get(name))
  override protected def setFunc(name: String, value: T): Unit = S.session.foreach(_.set(name, value))
  override protected def clearFunc(name: String): Unit = S.session.foreach(_.unset(name))

  def registerCleanupFunc(in: () => Unit): Unit =
  S.session.foreach(_.addSessionCleanup(ignore => in()))

}

/**
 * Keep request-local information around without the nastiness of naming session variables
 * or the type-unsafety of casting the results.
 * RequestVars share their value through the scope of the current HTTP
 * request.  They have no value at the beginning of request servicing
 * and their value is discarded at the end of request processing.  They
 * are helpful to share values across many snippets.
 *
 * @param dflt - the default value of the session variable
 */
abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) {

  override protected def findFunc(name: String): Box[T] = RequestVarHandler.get(name)
  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, value)
  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)

  def registerCleanupFunc(in: () => Unit): Unit =
  RequestVarHandler.addCleanupFunc(in)
}

object RequestVarHandler extends LoanWrapper {
  private val vals: ThreadGlobal[HashMap[String, Any]] = new ThreadGlobal
  private val cleanup: ThreadGlobal[ListBuffer[() => Unit]] = new ThreadGlobal
  private val isIn: ThreadGlobal[String] = new ThreadGlobal

  private[http] def get[T](name: String): Box[T] =
    for (ht <- Box.legacyNullTest(vals.value);
	 v <- ht.get(name).asInstanceOf[Option[T]]) yield v;


  private[http] def set[T](name: String, value: T): Unit =
    for (ht <- Box.legacyNullTest(vals.value))
      ht(name) = value

  private[http] def clear(name: String): Unit =
    for (ht <- Box.legacyNullTest(vals.value))
      ht -= name

  private[http] def addCleanupFunc(f: () => Unit): Unit =
    for (cu <- Box.legacyNullTest(cleanup.value))
      cu += f

  def apply[T](f: => T): T = {
    if ("in" == isIn.value)
    f
    else
    isIn.doWith("in") (
      vals.doWith(new HashMap) (
        cleanup.doWith(new ListBuffer) {
          val ret: T = f

          cleanup.value.foreach(f => Helpers.tryo(f()))

          ret
        }
      ))
  }
}


object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is
  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
}



