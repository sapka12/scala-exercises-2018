package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(tree: Tree[Int]): Int = ???
  def maximum(tree: Tree[Int]): Int = ???
  def depth(tree: Tree[Int]):Int = ???

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = ???
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = ???
}
