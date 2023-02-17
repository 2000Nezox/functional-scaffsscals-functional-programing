package gettingstarted

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](as: Tree[A]): Int =
    as match {
      case Leaf(x) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(as: Tree[Int]): Int =
    as match {
      case Leaf(x) => x
      case Branch(left, right) => maximum(left).max(maximum(right))
    }

  def depth[A](as: Tree[A]): Int =
    as match {
      case Leaf(x) => 1
      case Branch(left, right) => 1 + (depth(left)).max(depth(right))
    }

  def map[A, B](as: Tree[A])(f: (A) => B): Tree[B] =
    as match {
      case Leaf(x) => Leaf(f(x))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }


  def fold[A, B](f: A => B)(g: (B,B) => B): B =
    this match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(l.fold())
    }

  //  def size[A](as: Tree[A]): Int =
  //    as match {
  //      case Leaf(x) => 1
  //      case Branch(left, right) =>1 + size(left) + size(right)
  //    }
  //
  //  def maximum(as: Tree[Int]): Int =
  //    as match {
  //      case Leaf(x) => x
  //      case Branch(left, right) => maximum(left).max(maximum(right))
  //    }
  //
  //  def depth[A](as: Tree[A]): Int =
  //    as match {
  //      case Leaf(x) => 1
  //      case Branch(left, right) => 1 + (depth(left)).max(depth(right))
  //    }
  //
  //  def map[A,B](as: Tree[A])(f: (A) => B): Tree[B]=
  //    as match {
  //      case Leaf(x) => Leaf(f(x))
  //      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  //    }

}