package net.liftweb.util
import org.specs._
import scala.util.parsing.input._  
import org.specs.runner._
import scala.util.parsing.combinator.Parsers
import org.scalacheck._
import Gen._
import Prop._
import org.specs.matcher.ScalacheckParameters._

class CombParserHelpersSpecTest extends JUnit3(CombParserHelpersSpec)
object CombParserHelpersSpecRunner extends ConsoleRunner(CombParserHelpersSpec)
object CombParserHelpersSpec extends Specification {
  import ParserHelpers._
  "The parser helpers" should {
    "provide an isEol function returning true iff a char is end of line or end of file" in {
      isEol('\n') must beTrue
      isEol('\r') must beTrue
      isEol('\032') must beTrue
    }
    "provide an notEol function returning true iff a char is not end of line nor end of file" in {
      notEol('\n') must beFalse
      notEol('\r') must beFalse
      notEol('\032') must beFalse
    }
    "provide an isEof function returning true iff a char is end of file" in {
      isEof('\032') must beTrue
    }
    "provide an notEof function returning true iff a char is not end of file" in {
      notEof('\032') must beFalse
    }
    "provide an isNum function returning true iff a char is a digit" in {
      isNum('0') must beTrue
    }
    "provide an notNum function returning true iff a char is not a digit" in {
      notNum('0') must beFalse
    }
    "provide an wsc function returning true iff a char is a space character" in {
      List(' ', '\t', '\r', '\n') foreach { wsc(_) must beTrue }
      wsc('a') must beFalse
    }
    "provide a whitespace parser: white. Alias: wsc" in {
      import whiteStringGen._
      val whiteParse = (s: String) => white(s) must beLike { case Success(_, _) => true }
      property(whiteParse) must pass
    }
    "provide a whiteSpace parser always succeeding and discarding its result" in {
      import stringWithWhiteGen._
      val whiteSpaceParse = (s: String) => whiteSpace(s) must beLike { 
        case Success(x, y) => x.toString == "()"
        case _ => false
      }
      
      property(whiteSpaceParse) must pass
    }
  }
}
object whiteStringGen {
  def genWhiteString: Gen[String] = for (len <- choose(1, 4);
                                         string <- vectorOf(len, frequency((1, value(" ")), (1, value("\t")), (1, value("\r")), (1, value("\n"))))
                                    ) yield string.mkString("")
  implicit def whiteString(dummy: Arb[String]): Arbitrary[String] = new Arbitrary[String] {
    def getArbitrary = genWhiteString
  }
}
object stringWithWhiteGen {
  import whiteStringGen._
  implicit def genString(dummy: Arb[String]): Arbitrary[String] = new Arbitrary[String] {
    def getArbitrary = {
      for (len <- choose(1, 4);
           string <- vectorOf(len, frequency((1, value("a")), (2, value("b")), (1, genWhiteString)))
      ) yield string.mkString("")
   }
  }
}
object ParserHelpers extends CombParserHelpers with Parsers 