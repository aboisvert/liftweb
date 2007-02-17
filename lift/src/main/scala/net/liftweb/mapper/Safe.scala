package net.liftweb.mapper

/*                                                *\
 (c) 2006-2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */

import java.security.{SecureRandom, MessageDigest}
import org.apache.commons.codec.binary.Base64

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
  }
 
  /*
  def generateUniqueName = {
    S.nc+"_inp_"+randomString(15)+"_$"
  }
  */
  
  def hash(in : String) : String = {
    new String((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8")))
  }
  
  def hexDigest(in: Array[byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }
  
  def hexEncode(in: Array[byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[byte], pos: int, len: int, sb: StringBuilder) {
      if (pos < len) {
        val b: int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[char] else ('a' + (msb - 10)).asInstanceOf[char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[char] else ('a' + (lsb - 10)).asInstanceOf[char]))
                   
      addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }
}
