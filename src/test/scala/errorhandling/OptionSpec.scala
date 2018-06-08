package errorhandling

import org.scalatest.{FlatSpec, Matchers}
import Math.{sqrt => sq}
import datastructures.List

class OptionSpec extends FlatSpec with Matchers {

  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  val notNegative: Double => Boolean = _ >= 0

  def sqrt(a: Double): Option[Double] =
    if (notNegative(a)) Some(sq(a))
    else None

  "map" should "map inside Option" in {

    val some = Some("123")
    val none: Option[String] = None

    some.map(_.size) shouldBe Some(3)
    none.map(_.size) shouldBe None
  }

  "getOrElse" should "return the value inside Option or the default value" in {

    val some = Some("123")
    val none: Option[String] = None

    some.getOrElse("456") shouldBe "123"
    none.getOrElse("456") shouldBe "456"
  }

  "flatMap" should "handle a failing function" in {

    val somePos = Some(4.0)
    val someNeg = Some(-4.0)
    val none: Option[Double] = None

    somePos.flatMap(sqrt) shouldBe Some(2)
    someNeg.flatMap(sqrt) shouldBe None
    none.flatMap(sqrt) shouldBe None
  }

  "orElse" should "return the 1st Option if it is Some" in {

    val some = Some(4.0)
    val someOther = Some(1.0)
    val none: Option[Double] = None

    some.orElse(someOther) shouldBe some
    some.orElse(none) shouldBe some
    none.orElse(someOther) shouldBe someOther
    none.orElse(none) shouldBe none
  }

  "filter" should "filter by a predicate" in {

    val somePos = Some(4.0)
    val someNeg = Some(-1.0)
    val none: Option[Double] = None

    somePos.filter(notNegative) shouldBe somePos
    someNeg.filter(notNegative) shouldBe none
    none.filter(notNegative) shouldBe none
  }

  // http://www.alcula.com/calculators/statistics/variance/
  "variance" should "be calculated by def" in {
    Option.variance(Seq(10, 10, 20, 20)) shouldBe Some(25)
    Option.variance(Seq.empty) shouldBe None
  }

  "map2" should "map 2 Options int 1 by a func" in {
    def f(i: Int, s: String): Int = s.size * i

    Option.map2[Int, String, Int](Some(2), Some("hello"))(f) shouldBe Some(10)
    Option.map2[Int, String, Int](None: Option[Int], Some("hello"))(f) shouldBe None
    Option.map2[Int, String, Int](Some(2), None: Option[String])(f) shouldBe None
    Option.map2[Int, String, Int](None: Option[Int], None: Option[String])(f) shouldBe None
  }

  "sequence" should "convert a list of options into an option of list" in {
    Option.sequence(List()) shouldBe Some(List())
    Option.sequence(List(Some(1))) shouldBe Some(List(1))
    Option.sequence(List(Some(1), Some("1"))) shouldBe Some(List(1, "1"))
    Option.sequence(List(Some(1), None)) shouldBe None
    Option.sequence(List(None, Some(2))) shouldBe None
  }

  "traverse" should "convert a list into an option of list by a func" in {
    Option.traverse[String, Int](List("a", "b", "asdf"))(s => Some(s.size)) shouldBe Some(List(1, 1, 4))
    Option.traverse[Double, Double](List(1, 0, -1))(sqrt) shouldBe None
    Option.traverse[Double, Double](List(4, 1, 0))(sqrt) shouldBe Some(List(2, 1, 0))
  }

}
