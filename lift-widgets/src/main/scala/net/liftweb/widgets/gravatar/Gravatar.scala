package net.liftweb.widgets.gravatar

import scala.xml.{NodeSeq, Text, Group, Node, Elem}
import net.liftweb.http._
import net.liftweb.http.S
import net.liftweb.mapper._
import net.liftweb.http.S._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import java.util.Locale
import java.security._

// gravatar_id - MD5 sum of your email address
// size - image size
// rating - rating of the image, let's start with "G" which is also the default

class Gravatar {
  private def getMD5(s: String): String = {
    val m = MessageDigest.getInstance("MD5")
    m.update(s.getBytes(),0,s.length())
    BigInt(1,m.digest()).toString(16)
  }

  def render(e: String): NodeSeq = {
    var src = "http://www.gravatar.com/avatar.php?gravatar_id=" + getMD5(e)
    src = src + "&size=" + S.attr("s").openOr("42")
    src = src + "&rating=" + S.attr("r").openOr("G")
    
    <div id="gravatar_wrapper"><div id="gravatar_image"><img src={src}></img></div></div>
  }
}



