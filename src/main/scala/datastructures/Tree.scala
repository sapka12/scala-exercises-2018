package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))

  def size(tree: Tree[Int]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depth(tree: Tree[Int]):Int = fold(tree)(_ => 0)(_ max _ + 1)

}
