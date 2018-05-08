package datastructures

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.datastructures.List
import fpinscala.datastructures.List._

class ListSpec extends FlatSpec with Matchers {

  "tail" should "behave like tail" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
    tail(List()) shouldBe List()
  }

  "setHead" should "change the head element if exists" in {
    setHead(List(1, 2, 3), 5) shouldBe List(5, 2, 3)
    setHead(List(), 1) shouldBe List()
  }

  "takeWhile" should "behave like takeWhile" in {

    val f: Int => Boolean =  _ < 3

    takeWhile(List(1, 2, 3), f) shouldBe List(1, 2)
    takeWhile(List(1), f) shouldBe List(1)
    takeWhile(List(), f) shouldBe List()
  }

  "dropWhile" should "behave like dropWhile" in {

    val f: Int => Boolean =  _ < 3

    dropWhile(List(1, 2, 3), f) shouldBe List(3)
    dropWhile(List(3, 2, 1), f) shouldBe List(3, 2, 1)
    dropWhile(List(), f) shouldBe List()
  }

  "init" should "remove last element of the list" in {
    init(List(1, 2, 3)) shouldBe List(1, 2)
    init(List()) shouldBe List()
  }

  "length" should "count the elemts of the list" in {
    List.length(List(1, 2, 3)) shouldBe 3
    List.length(List()) shouldBe 0
  }

  behavior of "foldLeft"

  it should "sum" in {
    def add(x: Int, y: Int) = x + y
    foldLeft[Int, Int](List(1, 2, 3), 0)(add) shouldBe 6
    foldLeft[Int, Int](List(), 0)(add) shouldBe 0
  }

  it should "product" in {
    val X: (Int, Int) => Int = _ * _
    foldLeft[Int, Int](List(1, 2, 3), 0)(X) shouldBe 0
    foldLeft[Int, Int](List(1, 2, 3), 1)(X) shouldBe 6
    foldLeft[Int, Int](List(), 1)(X) shouldBe 1
  }

  "map" should "replace all elements in the list by the given function" in {
    val X: (Int, Int) => Int = _ * _
    val x3 = X(3, _)

    map(List(1, 2, 3))(x3) shouldBe List(3, 6, 9)
    map(List())(x3) shouldBe List()
  }

  "reduce" should "be similar to fold* but the in and out types are the same" in {
    reduce(List(1, 2, 3), 0)((x, y) => x + y) shouldBe 6
  }

}
