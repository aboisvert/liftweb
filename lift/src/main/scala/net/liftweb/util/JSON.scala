package net.liftweb.util;

import scala.util.parsing.combinator.{Parsers, ImplicitConversions, ~, mkTilde}
import Helpers._

trait JSONAny {
  implicit def strToStr(in: String) = in match {case null => JSONNull case in => JSONString(in)}
  implicit def boolToB(in: Boolean) = if (in) JSONTrue else JSONFalse
}

case class JSONCommand(command: String, params: List[JSONPair]) extends JSONAny

case class JSONObject(fields: List[JSONPair]) extends JSONAny {
  private val myMap = Map(fields.map(f => (f.name, f.value)) :_*)
  override def toString = fields.mkString("{ ", ", ", " }")
  def apply(name: String) = myMap(name)
  def get(name: String) = myMap.get(name)
  def elements = myMap.elements
  

}
case class JSONPair(name: String, value: JSONAny) {
  override def toString = name.encJs +": "+value
}
case class JSONString(value: String) extends JSONAny {
  override def toString = value.encJs
}
case class JSONNumber(value: Double) extends JSONAny {
  def this(in: Long) = this(in.toDouble)
  override def toString = value.toString
}
case class JSONArray(elems: List[JSONAny]) extends JSONAny {
  override def toString = elems.mkString("[ ", ", ", " ]")  
}
case object JSONTrue extends JSONAny {
  override def toString = "true"
}
case object JSONFalse extends JSONAny {
  override def toString = "false"
}
case object JSONNull extends JSONAny {
  override def toString = "null"
}

object JSONParser extends Parsers with ImplicitConversions { // with CombParserHelpers {
  implicit def strToInput(in: String): Input = new scala.util.parsing.input.CharArrayReader(in.toCharArray)
  type Elem = Char
  
  def whitespace = elem(' ') | elem('\t') | elem('\n') | elem('\r')
  
  def spaces = discard(rep(whitespace))
  
  def jsonObject = ( spaces ~ '{' ~ spaces ~ members ~ spaces ~ '}' ~ spaces ^^ {case xs =>
    xs.filter(i => i.name.equalsIgnoreCase("command") && i.value.isInstanceOf[JSONString]) match {
      case x :: _ => JSONCommand(x.value.asInstanceOf[JSONString].value.toLowerCase, xs.filter(!_.name.equalsIgnoreCase("command")))
      case _ => JSONObject(xs)
    }
    } )  |
    spaces ~'{' ~ spaces ~ '}' ~ spaces  ^^ JSONObject(Nil)
  
  def members = rep1sep(pair, pair, spaces ~ ',' ~ spaces)
  
  def pair = string ~ spaces ~ ':' ~ spaces ~ theValue ^^ {case s ~ v => JSONPair(s,v)}
  
  def string = ('\'' ~ rep(not('\'') ~ achar) ~ '\'' ^^ {case xs => xs.mkString("")}) |
               ('"' ~ rep(not('"') ~ achar) ~ '"' ^^ {case xs => xs.mkString("")})
               
  def achar = ('\\' ~ ('"' ^^ '"' | '\\' ^^ '\\' | '/' ^^ '/' | 'b' ^^ '\b' | 'n' ^^ '\n' | 'r' ^^ '\r' | 't' ^^ '\t' | 
    'u' ~ repN(4, hexDigit) ^^ {case dg => Integer.parseInt(dg.mkString(""), 16).toChar})) | elem("any char", c => c != '"' && c >= ' ')
  
  def number: Parser[JSONNumber] =  intFracExp | intFrac | intExp |  (anInt ^^ {case n => new JSONNumber(n)})
  
  def exp = e ~ digits ^^ {case x ~ d => d.mkString("").toInt * x}
  
  def e = ('e' ~ '-' ^^ -1) | ('e' ~ '+' ^^ 1) | ('e' ^^ 1) |
    ('E' ~ '-' ^^ -1) | ('E' ~ '+' ^^ 1) | ('E' ^^ 1)
  
  def intFracExp = anInt ~ frac ~ exp ^^ {case i ~ f ~ exp => JSONNumber((i.toString+"."+f+"e"+exp).toDouble)}
  
  def intFrac = anInt ~ frac ^^ {case i ~ f => JSONNumber((i.toString+"."+f).toDouble)}
  
  def intExp = anInt ~ exp ^^ {case i ~ e => JSONNumber((i.toString+"e"+e).toDouble)}
  
  def anInt = (digit19 ~ digits ^^ {case x ~ xs => (x :: xs).mkString("").toLong}) | 
    (digit ^^ {case x => x.toString.toLong}) | ('-' ~ digit19 ~ digits ^^ {case x ~ xs => (x :: xs).mkString("").toLong * -1L}) | 
      ('-' ~ digit ^^ {case x => x.toString.toLong * -1L})
  
  def digit19 = elem("digit", c => c >= '1' && c <= '9')
  
  def digit = elem("digit", c => c >= '0' && c <= '9')
  
  def hexDigit = elem("hex digit", c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
  
  def digits = rep1(digit)
  
  def frac = '.' ~ digits
    
  def theValue: Parser[JSONAny] = string ^^ JSONString | number | jsonObject | array | istrue | isfalse | isnull 
  
  def array = spaces ~ '[' ~ spaces ~ elements ~ spaces ~ ']' ~ spaces ^^ {case e => JSONArray(e)}
    
  def elements = repsep(theValue, spaces ~ ',' ~ spaces)
  
  def istrue = accept("true".toList) ^^ JSONTrue
  def isfalse = accept("false".toList) ^^ JSONFalse
  def isnull = accept("null".toList) ^^ JSONNull
  
  def parse(in: String) = theValue(in)
}
