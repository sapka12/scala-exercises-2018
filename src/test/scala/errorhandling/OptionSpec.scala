package errorhandling

import org.scalatest.{FlatSpec, Matchers}
import Math.{sqrt => sq}

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

}
