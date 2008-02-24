package net.liftweb.util

/*                                                *\
  (c) 2007 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
  \*                                                 */

trait LoanWrapper {
  def apply[T](f: => T): T
}
