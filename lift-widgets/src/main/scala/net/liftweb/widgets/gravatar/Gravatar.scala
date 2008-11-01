package net.liftweb.widgets.gravatar

import _root_.scala.xml.NodeSeq
import _root_.java.security._
import _root_.java.util._
import _root_.java.io._
import _root_.net.liftweb.util.Log


/**
 * To make a Gravatar: 
 *
 * <pre><code>
 * Gravatar("email@domain.com") // => Produces a gravatar thats 42x42 with a G rating
 * Gravatar("email@domain.com", 50) // => Produces a gravatar thats 50x50 with a G rating
 * Gravatar("email@domain.com", 50, "R") // => Produces a gravatar thats 50x50 with a R rating
 * </code></pre>
 *
 */
object Gravatar {
  
  val defaultSize: Int = 42
  val defaultRating: String = "G"
  val avatarEndpoint: String = "http://www.gravatar.com/avatar/"
  
  /**
   * @param e The email address of the recipient
   */
  def apply(e: String): NodeSeq = url(e,defaultSize,defaultRating)

  /**
   * @param e The email address of the recipient
   * @param s The square size of the output gravatar
   */
  def apply(e: String, s: Int): NodeSeq = url(e,s,defaultRating)
  
  /**
   * @param e The email address of the recipient
   * @param s The square size of the output gravatar
   * @param r The rating of the Gravater, the default is G
   */
  def apply(e: String, s: Int, r: String) = url(e,s,r)
  
  private def url(email: String, size: Int, rating: String): NodeSeq = {
    html(avatarEndpoint + getMD5(email) + "?s=" + size.toString + "&r=" + rating)
  }
  
  private def html(in: String): NodeSeq = {
    <div id="gravatar_wrapper"><div id="gravatar_image"><img src={in} alt="Gravater" /></div></div>
  }
  
  private def getMD5(message: String): String = {
    val md: MessageDigest = MessageDigest.getInstance("MD5")
    val bytes = message.getBytes("CP1252")
    
    try {
      BigInt(1,md.digest(bytes)).toString(16)
    } catch {
      case a: NoSuchAlgorithmException => Log.error("[Gravater] No Algorithm.", a); ""
      case x: UnsupportedEncodingException => Log.error("[Gravater] Unsupported Encoding.", x); ""
      case _ => Log.error("[Gravater] Unknown error."); ""
    }
  }
}
