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
package net.liftweb.http

import _root_.net.liftweb.util._
import Helpers._
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
  protected def wasInitialized(name: String): Boolean

  protected def __nameSalt = ""

  type CleanUpParam

  /**
   * The current value of the variable
   */
  def is: T = synchronized {
    findFunc(name) match {
      case Full(v) => v
      case _ => val ret = dflt
        testInitialized
        apply(ret)
        ret
    }
  }

  private def testInitialized: Unit = synchronized {
    if (!wasInitialized(name)) {
      registerCleanupFunc(_onShutdown _)
    }
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
  def apply(what: T): Unit = {
    testInitialized
    setFunc(name, what)
  }

  /**
   * Applies the given function to the contents of this
   * variable and sets the variable to the resulting value.
   *
   * @param f -- the function to apply and set the result from.
   */
  def update(f: T => T): T = {
    apply(f(is))
    is
  }

  def remove(): Unit = clearFunc(name)

  //def cleanupFunc: Box[() => Unit] = Empty

  private[http] def registerCleanupFunc(in: CleanUpParam => Unit): Unit

  protected final def registerGlobalCleanupFunc(in: CleanUpParam => Unit) {
    cuf ::= in
  }

  private var cuf: List[CleanUpParam => Unit] = Nil

  private def _onShutdown(session: CleanUpParam): Unit = {
    cuf.foreach(f => tryo(f(session)))
    onShutdown(session)
  }

  protected def onShutdown(session: CleanUpParam): Unit = {}

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

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name+"_inited_?"
    val old: Boolean = S.session.flatMap(_.get(bn)) openOr false
    S.session.foreach(_.set(bn, true))
    old
  }

  private[http] override def registerCleanupFunc(in: LiftSession => Unit): Unit =
  S.session.foreach(_.addSessionCleanup(in))

  type CleanUpParam = LiftSession
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
  type CleanUpParam = Box[LiftSession]
  override protected def findFunc(name: String): Box[T] = RequestVarHandler.get(name)
  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, value)
  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)
  override protected def wasInitialized(name: String): Boolean = {
    val bn = name+"_inited_?"
    val old: Boolean = RequestVarHandler.get(bn) openOr false
    RequestVarHandler.set(bn, true)
    old
  }

  override private[http] def registerCleanupFunc(in: Box[LiftSession] => Unit): Unit =
  RequestVarHandler.addCleanupFunc(in)
}

private[http] object RequestVarHandler /* extends LoanWrapper */ {
  private val vals: ThreadGlobal[HashMap[String, Any]] = new ThreadGlobal
  private val cleanup: ThreadGlobal[ListBuffer[Box[LiftSession] => Unit]] = new ThreadGlobal
  private val isIn: ThreadGlobal[String] = new ThreadGlobal
  private val sessionThing: ThreadGlobal[Box[LiftSession]] = new ThreadGlobal

  private[http] def get[T](name: String): Box[T] =
  for (ht <- Box.legacyNullTest(vals.value);
       v <- ht.get(name).asInstanceOf[Option[T]]) yield v;


  private[http] def set[T](name: String, value: T): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht(name) = value

  private[http] def clear(name: String): Unit =
  for (ht <- Box.legacyNullTest(vals.value))
  ht -= name

  private[http] def addCleanupFunc(f: Box[LiftSession] => Unit): Unit =
  for (cu <- Box.legacyNullTest(cleanup.value))
  cu += f

  def apply[T](session: Box[LiftSession], f: => T): T = {
    if ("in" == isIn.value) {
      sessionThing.set(session)
      f
    } else
    isIn.doWith("in") (
      vals.doWith(new HashMap) (
        cleanup.doWith(new ListBuffer) {
          sessionThing.doWith(session) {
            val ret: T = f

            cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

            ret
          }
        }
      ))
  }
}


object AnyVar {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is
  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is
}



