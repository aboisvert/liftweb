package net.liftweb.util
import org.specs._
import net.liftweb.util.Can._
import org.specs.runner._
import org.specs.Sugar._
import org.scalacheck.Gen._
import org.specs.matcher.ScalacheckParameters._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

class CanUnitTest extends JUnit3(CanUnit)
object CanUnitRunner extends ConsoleRunner(CanUnit)
object CanUnit extends Specification with CanGen {
  "A Can equals method" should {
    "return true with comparing two identical Can messages" in {
      val equality = (c1: Can[Int], c2: Can[Int]) => (c1, c2) match {
        case (Empty, Empty) => c1 == c2
        case (Full(x), Full(y)) => (c1 == c2) == (x == y)
        case (Failure(m1, e1, l1), Failure(m2, e2, l2)) => (c1 == c2) == ((m1, e1, l1) == (m2, e2, l2))
        case _ => c1 != c2
      }
      property(equality) must pass
    }
    "return false with comparing one Full and another object" in {
      Full(1) must_!= "hello"
    }
    "return false with comparing one Empty and another object" in {
      Empty must_!= "hello"
    }
    "return false with comparing one Failure and another object" in {
      Failure("", Empty, Nil) must_!= "hello"
    }
  }
}
trait CanGen {
  
  implicit def genThrowable(dummy: Arb[Throwable]): Arbitrary[Throwable] = new Arbitrary[Throwable] {
    case class UserException extends Throwable
    def getArbitrary = value(UserException())
  }

  implicit def genCan[T](dummy: Arb[Can[T]])(implicit a: Arb[T] => Arbitrary[T]): Arbitrary[Can[T]] = new Arbitrary[Can[T]] {
    def getArbitrary = frequency(
      (3, value(Empty)),
      (3, arbitrary[T].map(Full[T])),
      (1, genFailureCan)
    )
  }

  def genFailureCan: Gen[Failure] = for {
    msgLen <- choose(0, 4)
    msg <- vectorOf(msgLen, alphaChar)
    exception <- arbitrary[Can[Throwable]]
    chainLen <- choose(1, 5)
    chain <- frequency((1, vectorOf(chainLen, genFailureCan)), (3, value(Nil)))
  } yield Failure(msg.mkString, exception, chain.toList) 
 
}
