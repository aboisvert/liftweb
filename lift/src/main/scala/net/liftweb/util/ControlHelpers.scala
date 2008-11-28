package net.liftweb.util

/*
 * Copyright 2006-2008 WorldWide Conferencing, LLC
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


/**
 * Control helpers are providing alternate ways to catch exceptions and ignore them as necessary
 */
trait ControlHelpers extends ClassHelpers {

  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception with its class in the 'ignore' list or if 'ignore' is
   * null or an empty list, ignore the exception and return None.
   *
   * @param ignore list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception class in the list
   * @param onError optional callback function that will use the thrown exception as a parameter
   * @param f block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]], onError: Can[Throwable => Unit])(f: => T): Can[T] = {
    try {
      Full(f)
    } catch {
      case c if ignore.exists(_.isAssignableFrom(c.getClass)) => onError.foreach(_(c)); Empty
      case c if (ignore == null || ignore.isEmpty) => onError.foreach(_(c)); Failure("tryo", Full(c), Empty)
    }
  }

  /**
   * Wraps a "try" block around the function f
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](f: => T): Can[T] = tryo(Nil, Empty)(f)


  /**
   * Wraps a "try" block around the function f and trigger a callback function if an exception is thrown
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](onError: Throwable => Unit)(f: => T): Can[T] = tryo(Nil, Full(onError))(f)

  /**
   * Wraps a "try" block around the function f
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]])(f: => T): Can[T] = tryo(ignore, Empty)(f)

  /**
   * Wraps a "try" block around the function f. Takes only one Class of exception to ignore
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: Class[_])(f: => T): Can[T] = tryo(List(ignore), Empty)(f)
}


