package errorhandling

import java.lang.Math.{sqrt => sq}
import scala.{Either => _, Option => _, Some => _, List => _}

import datastructures.List
import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  val notNegative: Double => Boolean = _ >= 0

  def sqrt(a: Double): Either[String, Double] =
    if (notNegative(a)) Right(sq(a))
    else Left(s"$a is negative")

  "map" should "map inside Either" in {

    val right = Right(123)
    val left = Left("ERROR")

    right.map(_.toString) shouldBe Right("123")
    left.map(_.toString) shouldBe left
  }

  "flatMap" should "handle a failing function" in {

    val somePos = Right(4.0)
    val someNeg = Right(-4.0)
    val fail: Either[String, Double] = Left("oops")

    somePos.flatMap(sqrt) shouldBe Right(2)
    someNeg.flatMap(sqrt) shouldBe Left("-4.0 is negative")
    fail.flatMap(sqrt) shouldBe fail
  }

  "orElse" should "return the 1st Either if it is Right" in {

    val right = Right(4.0)
    val otherRight = Right(1.0)
    val left: Left[String] = Left("oops")
    val left2: Left[String] = Left("oops2")

    right.orElse(otherRight) shouldBe right
    right.orElse(left) shouldBe right
    left.orElse(otherRight) shouldBe otherRight
    left.orElse(left2) shouldBe left2
  }

  "traverse" should "convert a list into an option of list by a func" in {
    Either.traverse[String, Double, Double](List(1, 4))(sqrt) shouldBe Right(List(1, 2))
    Either.traverse[String, Double, Double](List(1, 0, -1))(sqrt) shouldBe Left("-1.0 is negative")
    Either.traverse[String, Double, Double](List(4, 1, 0))(sqrt) shouldBe Right(List(2, 1, 0))
  }

  "sequence" should "convert a list of options into an option of list" in {
    Either.sequence(List()) shouldBe Right(List())
    Either.sequence(List(Right(1))) shouldBe Right(List(1))
    Either.sequence(List(Right(1), Right("1"))) shouldBe Right(List(1, "1"))
    Either.sequence(List(Right(1), Left("oops"))) shouldBe Left("oops")
    Either.sequence(List(Left("oops"), Right(2))) shouldBe Left("oops")
    Either.sequence(List(Left("oops"), Left("oops2"))) shouldBe Left("oops")
  }
}
