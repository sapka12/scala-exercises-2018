package datastructures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  "size" should "count nodes" in {

    Tree.size(Leaf(123)) shouldBe 1
    Tree.size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  "maximum" should "retunr the max val from an Tree[Int]" in {

    Tree.maximum(Leaf(123)) shouldBe 123
    Tree.maximum(Branch(Leaf(1), Leaf(2))) shouldBe 2
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 3
  }

  "depth" should "return max path length from root to any leaf" in {

    Tree.depth(Leaf(123)) shouldBe 0
    Tree.depth(Branch(Leaf(1), Leaf(2))) shouldBe 1
    Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 2
  }

  "map" should "map the tre via f()" in {
    val abs: Int => Int = Math.abs(_)

    Tree.map(Leaf(123))(abs) shouldBe Leaf(123)
    Tree.map(Leaf(-123))(abs) shouldBe Leaf(123)
    Tree.map(Branch(Leaf(1), Branch(Leaf(-2), Leaf(3))))(abs) shouldBe Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

    val wordLength: String => Int = _.length

    Tree.map(Leaf("123"))(wordLength) shouldBe Leaf(3)
  }

  "fold" should "fold a tree via functions f() and g()" in {

    val wordLength: String => Int = _.length
    val sum: (Int, Int) => Int = _ + _

    Tree.fold(Leaf("asdf"))(wordLength, sum) shouldBe 4
    Tree.fold(Branch(Leaf("hello"), Leaf("world")))(wordLength, sum) shouldBe 10
    Tree.fold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(identity, sum) shouldBe 6
  }

}
