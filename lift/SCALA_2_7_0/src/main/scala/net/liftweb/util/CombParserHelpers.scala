package net.liftweb.util;

/* 
 * Copyright 2007 WorldWide Conferencing, LLC
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
import Helpers._
import scala.util.parsing.combinatorold.{Parsers, ImplicitConversions, ~, mkTilde}

/**  
 * The CombParserHelpers trait provides parser combinators helpers
 */
trait CombParserHelpers { self: Parsers =>

  /** the type of input elements defined in the Parsers trait is <code>Char</code>  */
  type Elem = Char

  /** @return a CharArray input build from a String  */
  implicit def strToInput(in: String): Input = new scala.util.parsing.input.CharArrayReader(in.toCharArray)

  /** @return true if the character is an end of line character or the end of file  */
  def isEol(c: Char): Boolean = (c == '\n' || c == '\r' || isEof(c))

  /** @return true if the character is not an end of line character nor the end of file  */
  def notEol(c: Char): Boolean = !isEol(c)

  /** @return true if the character is an end of file  */
  def isEof(c: Char): Boolean = c == '\032'

  /** @return true if the character is not an end of file  */
  def notEof(c: Char): Boolean = !isEof(c)

  /** @return true if the character is a digit  */
  def isNum(c: Char): Boolean = Character.isDigit(c)

  /** @return true if the character is not a digit  */
  def notNum(c: Char): Boolean = !isNum(c)

  /** @return true if the character is a space character  */
  def wsc(c: Char): Boolean = c == ' ' || c == '\n' || c == '\r' || c == '\t'

  /** @return a whitespace parser */
  def wsc: Parser[Elem] = elem("wsc", wsc)

  /** alias for the wsc parser */
  def white = wsc 
  
  /** @return a unit parser for any repetition of whitespaces */
  def whiteSpace = discard(rep(white))

  /** @return a parser accepting a 'line' space, either ' ' or '\t' */
  def aSpace = accept("whitespace", { case c if (c == ' ') || c == '\t' => true })

  /** @return a unit parser for any repetition of 'line' spaces */
  def lineSpace = discard(rep(aSpace))

  /**
   * @param elements list of characters 
   * @return a unit parser which will succeed if the input matches the list of characters regardless 
   * of the case (uppercase or lowercase)  
   */
  def acceptCI[ES <% List[Elem]](elements: ES): UnitParser = new UnitParser {
    def xform(in: Char): Char = Character.toUpperCase(in)
    def apply(in0: Input) = {
      var these: List[Elem] = elements
      var in = in0

      while(!these.isEmpty && xform(in.first) == xform(these.head)) {
        these = these.tail
        in = in.rest
      }

      if (these.isEmpty) Success((), in)
      else Failure("Expected: '"+these.head+"', found: '"+in.first+"'", in0)
    }                
  }
  implicit def ns(in: List[Elem]): String = in.mkString("").trim
  implicit def strToLst(in: String): List[Elem] = stringWrapper(in).toList
  implicit def ff(in: Parser[Elem]): List[Elem] = Nil

  def digit = elem("digit", isNum)

  def slash = elem("slash", c => c == '/')
  def dslash = discard(slash)

  def colon = elem("colon", c => c == ':')
  def dcolon = discard(colon)
  def EOL = discard(elem("EOL", isEol))


  def aNumber: Parser[Int] = rep1(elem("Number", isNum)) ^^ {case xs => xs.mkString("").toInt}
  

  def permute[T](p: (Parser[T])*): Parser[List[T]] = permute((lst : List[Parser[T]]) => lst.permute, p :_*)
  def permuteAll[T](p: (Parser[T])*): Parser[List[T]] = permute((lst : List[Parser[T]]) => lst.permuteAll, p :_*)

  def permute[T](func: List[Parser[T]] => List[List[Parser[T]]], p: (Parser[T])*): Parser[List[T]] = 
    if (p.isEmpty) 
      success(Nil);
    else {
      val right: Parser[List[T]] = success(Nil)
      
      p.toList match {
        case Nil => right
        case x :: Nil => x ~ right ^^ {case ~(x, xs) => x :: xs}
        case xs => func(xs).map(_.foldRight(right)(
          _ ~ _ ^^ {case ~(x, xs) => x :: xs})).
        reduceLeft((a: Parser[List[T]], b: Parser[List[T]]) => a | b)
      }
    }
  
  def repNN[T](n: Int, p: => Parser[T]): Parser[List[T]] = if (n == 0) rep(p) else p ~ repNN(n - 1, p) ^^ {case ~(x, xs) => x :: xs}  
}

trait SafeSeqParser extends Parsers {
  
  /** A parser generator for non-empty repetitions.
   *  
   * <p> rep1(f, p) first uses `f' (which must succeed) and then repeatedly uses `p' to 
   *     parse the input until `p' fails 
   *     (the result is a `List' of the consecutive results of `f' and `p')</p>
   *
   * @param first a `Parser' that parses the first piece of input
   * @param p a `Parser' that is to be applied successively to the rest of the input (if any)
   * @return A parser that returns a list of results produced by first applying `f' and then 
   *         repeatedly `p' to the input (it only succeeds if `f' matches).
   */
  override def rep1[T](first: => Parser[T], p: => Parser[T]): Parser[List[T]] = new Parser[List[T]] {
    def apply(in0: Input) = {
      val xs = new scala.collection.mutable.ListBuffer[T]
      var in = in0
      
      var res = first(in)
      
      while(res.successful) {
        xs += res.get
        in = res.next
        res = p(in)
      }
      
      if (!xs.isEmpty) Success(xs.toList, res.next)
      else Failure("TODO", in0)
    }
  }
  
}
