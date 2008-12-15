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

import _root_.java.net.{URLDecoder, URLEncoder}
import _root_.scala.collection.mutable.{HashSet, ListBuffer}
import _root_.scala.xml.{NodeSeq, Elem, Node, Text, Group, UnprefixedAttribute, Null, Unparsed, MetaData, PrefixedAttribute}
import _root_.scala.collection.{Map}
import _root_.scala.collection.mutable.HashMap
import _root_.java.util.concurrent.atomic.AtomicLong

object HttpHelpers extends ListHelpers with StringHelpers

trait HttpHelpers { self: ListHelpers with StringHelpers  =>

  /**
   * URL decode the string.
   *
   * This is a pass-through to Java's URL decode with UTF-8
   */
  def urlDecode(in : String) = URLDecoder.decode(in, "UTF-8")

  /**
   * URL encode the string.
   *
   * This is a pass-through to Java's URL encode with UTF-8
   */
  def urlEncode(in : String) = URLEncoder.encode(in, "UTF-8")

  /**
   * Take a list of name/value parse and turn them into a URL query string
   *
   * @param params the name/value pairs
   * @return a valid query string
   */
  def paramsToUrlParams(params: List[(String, String)]): String = params.map {
    case (n, v) => urlEncode(n) + "=" + urlEncode(v)
  }.mkString("&")


  /**
   * Append parameters to a URL
   *
   * @param url the url to append the params to
   * @param params the parameters (name/value) to append to the URL
   *
   * @return the url with the parameters appended
   */
  def appendParams(url: String, params: Seq[(String, String)]): String = params.toList match {
    case Nil => url
    case xs if !url.contains("?") => url + "?" + paramsToUrlParams(xs)
    case xs => url + "&" + paramsToUrlParams(xs)
  }

  /*
   /**
    * Set of all valid files extensions
    * @return a mutable HashSet[String]
    */
   val validSuffixes = {
   val ret = new HashSet[String]
   ret += ("png", "js", "css", "jpg", "ico", "gif", "tiff", "jpeg")
   ret
   }

   /**
    * Test if a path starts with "/", doesn't contain "/." and contains a valid suffix
    */
   def goodPath_?(path : String): Boolean = {
   if (path == null || path.length == 0 || !path.startsWith("/") || path.indexOf("/.") != -1) false
   else {
   val lastPoint = path.lastIndexOf('.')
   val lastSlash = path.lastIndexOf('/')
   if (lastPoint <= lastSlash) false else {
   validSuffixes.contains(path.substring(lastPoint + 1))
   }
   }
   }
   */
  /**
   * get a map of HTTP properties and return true if the "Content-type"
   * is either "text/html" or "application/xhtml+xml"
   * @param in Map which may contain a key named Content-Type
   * @return true if there is a pair ("Content-Type", "text/html") or
   *                                 ("Content-Type", "application/xhtml+xml")
   */
  def couldBeHtml(in: Map[String, String]): Boolean =
  in match {
    case null => true
    case n => {
        n.get("Content-Type") match {
          case Some(s) => { (s.toLowerCase == "text/html") ||
                           (s.toLowerCase == "application/xhtml+xml") }
          case None => true
        }
      }
  }

  /**
   * Return true if the xml doesn't contain an <html> tag
   */
  def noHtmlTag(in: NodeSeq): Boolean = (in \\ "html").length != 1

  /**
   * Transform a general Map to a nutable HashMap
   */
  def toHashMap[A,B](in : Map[A,B]) : HashMap[A,B] = {
    val ret = new HashMap[A,B];
    in.keys.foreach { k => ret += Pair(k, in(k)) }
    ret
  }

  /**
   * Insure all the appropriate fields are in the header
   */
  def insureField(toInsure: List[(String, String)], headers: List[(String, String)]): List[(String, String)] = {
    def insureField_inner(toInsure : List[(String, String)], field : (String, String)): List[(String, String)] =
    toInsure.ciGet(field._1) match {
      case Full(_) => toInsure
      case _ => field :: toInsure
    }

    headers match {
      case Nil => toInsure
      case x :: xs => insureField(insureField_inner(toInsure, x), xs)
    }
  }


  /**
   * Transform a pair (name: String, value: Any) to an unprefixed XML attribute name="value"
   */

  implicit def pairToUnprefixed(in: (String, Any)): MetaData = {
    val value: Option[NodeSeq] = in._2 match {
      case null => None
      case js: ToJsCmd => Some(Text(js.toJsCmd))
      case n: Node => Some(n)
      case n: NodeSeq => Some(n)
      case None => None
      case Some(n: Node) => Some(n)
      case Some(n: NodeSeq) => Some(n)
      case Empty => None
      case Full(n: Node) => Some(n)
      case Full(n: NodeSeq) => Some(n)
      case s => Some(Text(s.toString))
    }

    value.map(v => new UnprefixedAttribute(in._1, v, Null)) getOrElse Null
  }


  /**
   * If the incoming Elem has an 'id', return it, otherwise
   * construct a new Elem with a randomly generated id and return the pair
   *
   * @param in the element to test & add 'id' to
   *
   * @return the new element and the id
   */
  def findOrAddId(in: Elem): (Elem, String) = (in \ "@id").toList match {
    case Nil => {
        val id = nextFuncName
        (in % ("id" -> id), id)
      }
    case x :: xs => (in, x.text)
  }

  private val serial = new AtomicLong(Math.abs(Helpers.randomLong(Helpers.millis)))

  /**
   * Get a monotonically increasing number that's guaranteed to be unique for the
   * current session
   */
  def nextNum = serial.incrementAndGet

  /**
   * Get a guanateed unique field name
   * (16 or 17 letters and numbers, starting with a letter)
   */
  def nextFuncName = {
    val sb = new StringBuilder(20)
    sb.append('F')
    sb.append(nextNum)
    // sb.append('_')
    sb.append(randomString(3))
    sb.toString
  }
		   

  private case class BailOut(seq: Long)
  import _root_.scala.actors._
  import Actor._
  def longPoll[T](seq: Long, timeout: Helpers.TimeSpan, func: PartialFunction[Any, T]): Can[T] = {
    ActorPing.schedule(Actor.self, BailOut(seq), timeout)
    receive(func orElse {case BailOut(seq) => null}) match {
      case null => Empty
      case r: T => Full(r)
    }
  }

}

/**
 * Is this something that can be converted to a JavaScript Command
 */
trait ToJsCmd {
  def toJsCmd: String
}
