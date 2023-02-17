package gettingstarted

sealed trait List[+A]

case object Nil1 extends List[Nothing]

case class Cons1[+A](head: A, tail2: List[A]) extends List[A]

object List1 {
  def sum(ints: List[Int]): Int = ints match {
    case Nil1 => 0
    case Cons1(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil1 => 1.0
    case Cons1(0.0, _) => 0.0
    case Cons1(x, xs) => x * product(xs)
  }

  def tail1[A](l: List[A]): List[A] =
    l match {
      case Nil1 => sys.error("tail of empty list")
      case Cons1(_, t) => t
    }

  def setHeader[A](x: A, l: List[A]): List[A] =
    l match {
      case Nil1 => sys.error("tail of empty list")
      case Cons1(_, _) => Cons1(x, l)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil1 => sys.error("empty list")
      case Cons1(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons1(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil1 => z
      case Cons1(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil1 => z
      case Cons1(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => 1 + b)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil1: List[A])((x, y) => Cons1(y, x))

  def addAll(as: List[Int]): List[Int] =
    foldRight(as, Nil1: List[Int])((x, y) => Cons1(x + 1, y))

  def consToString(as: List[Double]): List[String] =
    foldRight(as, Nil1: List[String])((x, y) => Cons1(x.toString, y))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil1: List[B])((x, y) => Cons1(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil1: List[A])((x, y) => if (f(x)) Cons1(x, y) else y)
  }


  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons1(x, Nil1) => Nil1
      case Cons1(h, t) => Cons1(h, init(t))
    }
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil1
    else Cons1(as.head, apply(as.tail: _*))
}
