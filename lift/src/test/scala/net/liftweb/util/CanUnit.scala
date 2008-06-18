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

import org.specs._
import net.liftweb.util.Can._
import org.specs.runner._
import org.specs.Sugar._
import org.specs.Scalacheck
import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.property

class CanUnitTest extends Runner(CanUnit) with JUnit
object CanUnit extends Specification with CanGen with Scalacheck {
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

  implicit def genThrowable: Arbitrary[Throwable] = Arbitrary[Throwable] {
    case class UserException extends Throwable
    value(UserException())
  }

  implicit def genCan[T](implicit a: Arbitrary[T]): Arbitrary[Can[T]] = Arbitrary[Can[T]] {
    frequency(
      (3, value(Empty)),
      (3, a.arbitrary.map(Full[T])),
      (1, genFailureCan)
    )
  }

  def genFailureCan: Gen[Failure] = for {
    msgLen <- choose(0, 4)
    msg <- vectorOf(msgLen, alphaChar)
    exception <- value(Full(new Exception("")))
    chainLen <- choose(1, 5)
    chain <- frequency((1, vectorOf(chainLen, genFailureCan)), (3, value(Nil)))
  } yield Failure(msg.mkString, exception, chain.toList)

}
