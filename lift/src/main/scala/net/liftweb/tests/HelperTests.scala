package net.liftweb.tests

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import scala.testing.SUnit
import SUnit._

import net.liftweb.util.Log
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._
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
    Mailer.sendMail(From("test@liftweb.net"), Subject( "Testing lift's mail sending at "+tn),To("test@liftweb.net"),"Dude... this is kewl! @"+tn,
        <html><body>Dude... <b>this</b> is kewl<i>! @{tn}</i></body></html>)
    Mailer.sendMail(From("test@liftweb.net"), Subject( "Testing lift's mail sending at "+tn),To("test@liftweb.net"),
        XHTMLPlusImages(<html><body>This is an <a href='http://liftweb.net'><img src="cid:foomoo" /></a> image</body></html>, PlusImageHolder("foomoo", "image/gif", base64Decode(picture))))
        
    Thread.sleep(100) // give the background thread a chance to send the message
    
    assert(try {
      processString("Hello <%= mrdog %> how are you", Map("mrcat" -> "meow"))
      false
    } catch {
      case e: Exception => true
    })
    assert(processString("Hello <%= mrdog %> how are you", Map("mrdog" -> "meow")).indexOf("meow") > 4)

    Log.warn("Hello world")
    
    val key = makeBlowfishKey
    val theMsg = (1 to 100000).mkString("(", ",", ")")
    val msgBytes = theMsg.getBytes
    val enc = readWholeStream(encryptStream(new java.io.ByteArrayInputStream(msgBytes), key))
    assert(notEq(enc, msgBytes), "The encypted stream is different, but the len is the same "+enc.length+" "+msgBytes.length+" str "+(new String(enc)))
    val dec = new String(readWholeStream(decryptStream(new java.io.ByteArrayInputStream(enc), key)))
    assert (dec == theMsg, "We encrypted and decrypted it")
    
  }
  
  val picture = """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"""
}
