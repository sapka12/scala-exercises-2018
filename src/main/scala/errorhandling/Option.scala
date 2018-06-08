package errorhandling

import datastructures.{Cons, List}

import scala.{Either => _, Option => _, Some => _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(mean =>
    if (xs.isEmpty) None
    else Some {
      def meanDiffSq(x: Double) = (x - mean) * (x - mean)

      val sumSqs = xs.map(meanDiffSq).sum
      sumSqs / xs.size
    })

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    List.foldRight[Option[A], Option[List[A]]](a, Some(List())) { (optA, optListA) =>
      optA.flatMap { a =>
        optListA.map { listA =>
          Cons(a, listA)
        }
      }
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(List.map(a)(f))
}
