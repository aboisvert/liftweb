package net.liftweb.util
import org.specs._
import net.liftweb.util.Can._
import org.specs.runner._
import org.specs.Sugar._
import org.specs.util.DataTables
import org.scalacheck.Gen._
import org.specs.matcher.ScalacheckParameters._

class CanUnitTest extends JUnit3(CanUnit)
object CanUnitRunner extends ConsoleRunner(CanUnit)
object CanUnit extends Specification with DataTables with CanValues {
  "A Can equals method" should {
    "return true with comparing 2 Full having the same value" in {
      Full(1) must_== Full(1)
    }
    "return true with comparing 1 Full and its value" in {
      Full(1) must_== 1
    }
    "return true with comparing 2 Empty" in {
      Empty must_== Empty
    }
    "return false with comparing one Full and one Empty" in {
      Full(1) must_!= Empty
      Empty must_!= Full(1)
    }
    "return false with comparing one Full and another object" in {
      Full(1) must_!= "hello"
    }
    "return false with comparing one Empty and another object" in {
      Empty must_!= "hello"
    }
    "return true with comparing two identical Failure messages" in {
      "message" | "exception" | "chain"    |
      "msg"     ! empty       !  nil       | 
      "msg"     ! full        !  chainOf2  | 
      "msg"     ! full        !  nil       | 
      "msg"     ! full        !  chainOf1  |> {(msg: String, exception: Can[Throwable], chain: List[Failure]) =>
         Failure(msg, exception, chain) must_== Failure(msg, exception, chain) 
      }
    }
    "return false with comparing two different Failure messages" in {
      val failureData = for (m1 <- elements("msg", "msg2");
                             m2 <- elements("msg", "msg2") if (m1 != m2);
                             e1 <- elements(empty, full);
                             e2 <- elements(empty, full) if (e1 != e2);
                             c1 <- elements(nil, chainOf1, chainOf2);
                             c2 <- elements(nil, chainOf1, chainOf2) if (c1 != c2))
                             yield (m1, e1, c1, m2, e2, c2)

      failureData must pass { t: (String, Can[Throwable], List[Failure], String, Can[Throwable], List[Failure]) => 
        val (msg, exception, chain, otherMsg, otherEx, otherChain) = t
        Failure(msg, exception, chain) must_!= Failure(otherMsg, otherEx, otherChain) 
      }(set(minTestsOk -> 50))
    }
  }
}
trait CanValues {
  case class LiftException extends Exception
  val empty: Can[LiftException] = Empty
  val full: Can[LiftException] = Full(LiftException())
  val nil: List[Failure] = Nil
  val chainOf2 = List(Failure("e", Full(LiftException()), Nil))
  val chainOf1 = List(Failure("e", Empty, Nil))
}
