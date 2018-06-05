package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A, B](tree: Tree[A])(implicit map: A => B, reduce: (B, B) => B): B = tree match {
    case Leaf(a) => map(a)
    case Branch(l, r) => reduce(fold[A, B](l), fold[A, B](r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)), Branch(_, _))

  def size(tree: Tree[Int]): Int = fold[Int, Int](tree)(_ => 1, _ + _ + 1)

  def maximum(tree: Tree[Int]): Int = fold[Int, Int](tree)(identity, _ max _)

  def depth(tree: Tree[Int]):Int = fold[Int, Int](tree)(_ => 0, _ max _ + 1)

}
