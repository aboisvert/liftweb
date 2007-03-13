package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.security.{SecureRandom, MessageDigest}
import org.apache.commons.codec.binary.Base64
import net.liftweb.util.Helpers

/**
 * Manage the current "safety" state of the stack
 */
object Safe {
  private val rand = new SecureRandom
  /**
   * Get the next "safe" number
   */
  def next = rand.nextLong
  private val threadLocal = new ThreadLocal
  
  /**
   * Is the current context "safe" for the object with the
   * given safety code?
   */
  def safe_?(test : long) : boolean = test == threadLocal.get

  /**
   * Marks access to a given object as safe for the duration of the function
   */
  def runSafe[T](x : long)(f : => T) : T = {
    // FIXME -- this should collect all the objects that have been marked safe such that
    // multiple objects can be safe at the same time
    val old = threadLocal.get
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(old)
    }
  }
  

  def randomString(len: int) = Helpers.randomString(len)

  /*
   def randomString(len : int) : String = {
   len match {
   case 0 => ""
   case _ => randomChar + randomString(len - 1)
   }
   }
   
   def randomChar : String = {
   rand.nextInt(62) match {
   case s @ _ if (s < 26) => {('A' + s).asInstanceOf[char].toString}      
   case s @ _ if (s < 52) => {('a' + (s - 26)).asInstanceOf[char].toString}      
   case s @ _  => {('0' + (s - 52)).asInstanceOf[char].toString}      
   }
   }*/
  
  /*
   def generateUniqueName = {
   S.nc+"_inp_"+randomString(15)+"_$"
   }
   */

}
