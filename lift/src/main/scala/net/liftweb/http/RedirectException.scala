package net.liftweb.http

/*                                                *\
  (c) 2007-2008 WorldWide Conferencing, LLC
  Distributed under an Apache License
  http://www.apache.org/licenses/LICENSE-2.0
\*                                                 */

import net.liftweb.util._

class RedirectException(val msg: String,val to: String,val func: Can[() => Unit]) extends Exception(msg) {

}
