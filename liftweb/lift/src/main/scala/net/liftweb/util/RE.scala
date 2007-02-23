package net.liftweb.util

/*                                                *\
 (c) 2007 WorldWide Conferencing, LLC
 Distributed under an Apache License
 http://www.apache.org/licenses/LICENSE-2.0
 \*                                                 */


import java.util.regex.{Pattern, Matcher}
import scala.collection.mutable.ArrayBuffer

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
  def apply(in: String) = new REDoer(in)

  implicit def matchResToBoolean(in: REMatcher): boolean = {
    in match {
      case null => false
      case _ => in.matches
    }
  }

  class SuperString(val str: String) {
    def substring(re: REDoer) = re.=~(str).matchStr
}

  implicit def strToSuperStr(in: String): SuperString = new SuperString(in)
}

class REDoer(val pattern: String) {
  private val compiled = Pattern.compile(pattern)
  
  def =~(other: String) = {
    new REMatcher(other, compiled)
  }
  
  def =~:(other: String) = {
    new REMatcher(other, compiled)
  }
}

class REMatcher(val str: String,val compiled: Pattern) {
  private val matcher = compiled.matcher(str)
  
  
  val matches = matcher.find

  val matchStr = if (matches) str.substring(matcher.start, matcher.end)
                 else ""

  def capture= map{s => s}

  def map[T](f : (String) => T): Iterator[T] = {
    val ab = new ArrayBuffer[T]
    matcher.reset
    val cnt = matcher.groupCount

    if (cnt > 0) {
      while (matcher.find) {
        var pos = 0
        var sb = new ArrayBuffer[String]

        while (pos < cnt) {
          ab += f(matcher.group(pos + 1))
          pos = pos + 1
        }
        
      }
    }
    
    ab.elements
  }
}


