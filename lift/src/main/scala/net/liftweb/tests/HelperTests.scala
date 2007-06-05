package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.testing.SUnit
import SUnit._

import net.liftweb.util.Log
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.mapper._
import net.liftweb.proto._

class HelperTests extends TestCase("Helper Tests") {
  override def runTest() {
    assert(toInt("1") == 1)
    assert(toInt(Some(1)) == 1)
    assert(toInt(None) == 0)
    assert(toInt(new java.lang.Double(1.0)) == 1)
    assert(toInt(33 :: Nil) == 33)
    assert(toInt(33L :: Nil) == 33)
    assert(toBoolean(false) == false)
    assert(toBoolean(true) == true)
    assert(toBoolean("true") == true)
    assert(toBoolean("TRUE") == true)
    assert(toBoolean("t") == true)
    assert(toBoolean("false") == false)
    assert(toBoolean(1) == true)
    assert(toBoolean(0L) == false)
    assert(toBoolean(Some(true)) == true)
    assert(toBoolean(None) == false)
    assert(toBoolean(Some(33)) == true)
    assert(toBoolean(Some(false)) == false)
    assert(toBoolean(Some("true" :: Nil)) == true)
    assert(toBoolean(1 :: Nil) == true)
    assert(toBoolean(0 :: Nil) == false)
    
    assert(seconds(3) == 3000L)
    assert(3.seconds == 3000L)
    assert((4 minutes) + (3 seconds) == (3000L + 4L * 60L * 1000L))
    assert(17.minutes == (17L * 60L * 1000L))
    assert(4.hours == (4L * 60L * 60L * 1000L))
    assert((3 days) == (3L * 24L * 60L * 60L * 1000L))
    assert(52.weeks == (52L * 7L * 24L * 60L * 60L * 1000L))
    
    val min5:long = (5.minutes.later - System.currentTimeMillis) - 5.minutes
    
    assert(min5 < 2L)
    
    assert((5.minutes.ago - System.currentTimeMillis) + 5.minutes < 2L)
    
    val tn = timeNow.toString
    sendMail("test@liftweb.net", List("test@liftweb.net"), "Testing lift's mail sending at "+tn,Nil,"Dude... this is kewl! @"+tn,
        <html><body>Dude... <b>this</b> is kewl<i>! @{tn}</i></body></html>)
    Thread.sleep(100) // give the background thread a chance to send the message
    
    assert(try {
      processString("Hello <%= mrdog %> how are you", Map("mrcat" -> "meow"))
      false
    } catch {
      case e: Exception => true
    })
    assert(processString("Hello <%= mrdog %> how are you", Map("mrdog" -> "meow")).indexOf("meow") > 4)

    Log.warn("Hello world")
  }
}
