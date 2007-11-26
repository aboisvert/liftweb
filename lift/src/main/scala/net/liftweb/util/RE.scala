package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */


import java.util.regex.{Pattern, Matcher}
import scala.collection.mutable.ListBuffer

/**
 * A regular expressions helper library
 * RE("foo") =~ "A string" -or-
 * "A string" =~: RE("foo") -or-
 * "A String".substring(RE("Str"))
 * ("A B cat D" =~: RE("([A-Z])")).capture // List(A,B,D)
 */
object RE {
  /**
   * Create a regular expression from a String
   */
  def apply(in: String) = new REDoer(in, Empty)
  def apply[T](in: String, func: PartialFunction[(T, List[String]), T]) = new REDoer(in, Full(func))

  implicit def matchResToBoolean(in: REMatcher): boolean = {
    in match {
      case null => false
      case _ => in.matches
    }
  }

  class SuperString(val str: String) {
    def substring(re: REDoer[_]) = re.=~(str).matchStr
  }

  implicit def strToSuperStr(in: String): SuperString = new SuperString(in)
  implicit def strToRe(in: String): REDoer[Nothing] = new REDoer(in, Empty)
}

class REDoer[T](val pattern: String,val func: Can[PartialFunction[(T, List[String]), T]]) extends Function2[T, String, Can[T]] {
  val compiled = Pattern.compile(pattern)
  
  def =~(other: String) = {
    new REMatcher(other, compiled)
  }
  
  def =~:(other: String) = {
    new REMatcher(other, compiled)
  }  
  
  def apply(obj: T, other: String): Can[T] = {
    val ma = new REMatcher(other, compiled)
    if (!ma.matches) Empty
    else func.flatMap(f => if (f.isDefinedAt((obj, ma.capture))) Full(f((obj, ma.capture))) else Empty)
  }

}

object REMatcher {
  def unapply(in: REMatcher): Option[List[String]] = Some(in.capture)
}

class REMatcher(val str: String,val compiled: Pattern) {
  private val matcher = compiled.matcher(str)
  
  lazy val matches = matcher.find

  lazy val matchStr: Can[String] = if (matches) Full(str.substring(matcher.start, matcher.end))
                 else Empty

  lazy val capture = map(s => s)
  
  def foreach(func: (String, List[String]) => Unit): Unit = 
  {
     var pos = 0
     matcher.reset
     val m = matcher
     while (matcher.find) {
       func(str.substring(pos, m.start), (0 to m.groupCount).toList.map(i => m.group(i)))
       pos = matcher.end
     }
     func(str.substring(pos), Nil)
  }

  def map[T](f : (String) => T): List[T] = synchronized {
    val ab = new ListBuffer[T]
    matcher.reset
    val cnt = matcher.groupCount
    
    def doIt {
      def runIt(pos: Int) {
        if (pos >= cnt) return
        else {ab += f(matcher.group(pos + 1)) ; runIt(pos + 1)}
      }
      
      if (!matcher.find) return
      else {runIt(0) ; doIt}
    }

    doIt
    
    ab.toList
  }
  
}


