/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.util

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

import java.io._
import scala.util.matching._
import scala.collection.mutable.{HashSet}  

object CSSHelpers extends ControlHelpers {
  
  /**
   * Adds a prefix to root relative paths in the url segments from the css content
   * 
   * @param in - the text reader
   * @param rootPrefix - the prefix to be added
   */
  def fixCSS(in: BufferedReader, rootPrefix: String): Can[String] = {
    tryo ((t: Throwable) => Log.error("ERROR ", t)) {
      val tokens = new HashSet[String]();
      val res = new StringBuilder;
      var line: String = null;
 
      while ({line = in.readLine(); line != null}) {
        res append line + "\n"
      }
      
     val css = res toString;
     tokens ++= "url\\([^\\)]*\\)".r.findAllIn(css).
       map(URLParser.parse(_)).
       filter( !_.isEmpty ).
       map(_ open_!)
    
     (css /: tokens)((left, right) => left.replaceAll(right,  rootPrefix + right))
     
    } 
  }
  
  
  /**
   * Fixes a CSS content provided by source parameter and writes int into dest
   * @source - the CSS reader
   * @dest - where to put the transformed CSS
   * @rootPefix - the prefix to be added to root relative URL's
   */
  def fixCSSAndWrite(source: Reader, dest: Writer, rootPrefix: String) {
    tryo ((t: Throwable) => Log.error("ERROR ", t)) {
      fixCSS(new BufferedReader(source), rootPrefix) map (css => {
          val pw = new PrintWriter(dest);
          pw.print(css);
          pw.flush
        }
      )
    }
    tryo {source close()}
    tryo {dest close()}

  }
  
}

/**
 * Combinator parser for extracting the url fragment as per CSS2 spec
 * <br/>
 * <i>
 * The format of a URI value is 'url(' followed by optional whitespace followed 
 * by an optional single quote (') or double quote (") character followed by the URI 
 * itself, followed by an optional single quote (') or double quote (") character followed 
 * by optional whitespace followed by ')'. The two quote characters must be the same.
 * </i>
 */
object URLParser extends Parsers with ImplicitConversions {
  implicit def strToInput(in: String): Input = new scala.util.parsing.input.CharArrayReader(in.toCharArray)
  type Elem = Char

  lazy val spaces = (elem(' ') | elem('\t') | elem('\n') | elem('\r')).*
  lazy val url = acceptSeq("url" toList)
  // consider only root relative paths that start with /
  lazy val path = elem('/') ~> elem("path", c => c.isLetterOrDigit ||
                         c == '?' || c == '/' ||
                         c == '&' || c == '@' ||
                         c == ';' || c == '.' || 
                         c == '+' || c == '-' || 
                         c == '=' || c == ':' ).+ ^^ {case l => l.mkString("")}
  
  // the URL might be wrapped in simple quotes
  lazy val seq1 = elem('\'') ~> path <~ elem('\'')
  // the URL might be wrapped in double quotes
  lazy val seq2 = elem('\"') ~> path <~ elem('\"')
  // do the parsing per CSS spec http://www.w3.org/TR/REC-CSS2/syndata.html#uri section 4.3.4
  lazy val expr = (url ~ spaces ~ elem('(') ~ spaces) ~> ( seq1 | seq2 | path ) <~ (spaces <~ elem(')'))

  
  def parse(in: String): Can[String] = expr(in) match {
    case Success(v, _) => Full("/" + v)
    case _ => Empty
  }

}