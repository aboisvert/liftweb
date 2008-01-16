package net.liftweb.util;

import Helpers._
import scala.util.parsing.combinator.{Parsers, ImplicitConversions, ~, mkTilde}

trait CombParserHelpers { self: Parsers =>
implicit def strToInput(in: String): Input = new scala.util.parsing.input.CharArrayReader(in.toCharArray)
type Elem = Char

def notEol(c: Char): Boolean = c != '\n' && notEOF(c)
def isEol(c: Char): Boolean = (c == '\n' || c == '\r' || isEOF(c))
  def notNum(c: Char): Boolean = !Character.isDigit(c)
def isNum(c: Char): Boolean = Character.isDigit(c)
def notEOF(c: Char): Boolean = c != '\032'
def isEOF(c: Char): Boolean = c == '\032'
def wsc(c: Char): Boolean = c == ' ' || c == '\n' || c == '\r' || c == '\t'

def acceptCI[ES <% List[Elem]](es: ES): UnitParser = new UnitParser {
  def xform(in: Char): Char = Character.toUpperCase(in)
  def apply(in0: Input) = {
    var res = new scala.collection.mutable.ListBuffer[Elem]
    var these: List[Elem] = es
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
def white = wsc 

def digit = elem("digit", isNum)

def slash = elem("slash", c => c == '/')
def dslash = discard(slash)

def colon = elem("colon", c => c == ':')
def dcolon = discard(colon)
def EOL = discard(elem("EOL", isEol))

def whiteSpace = discard(rep(white))
def wsc: Parser[Elem] = elem("wsc", wsc)

  def aNumber: Parser[Int] = rep1(elem("Number", isNum)) ^^ {case xs => xs.mkString("").toInt}
  
  def aSpace = accept("whitespace",{ case c if (c == ' ') || c == '\t' => true })
  
  def lineSpace = discard(rep(aSpace))

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
