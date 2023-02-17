import gettingstarted.List1.{init, length, sum, filter}
import gettingstarted.{Cons1, List1, Nil1}

object Hello extends App {
  println("Hello, World!")

  val x = List1(1, 2, 3, 4, 5) match {
    case Cons1(x, Cons1(2, Cons1(4, _))) => x
    case Nil1 => 42
    case Cons1(x, Cons1(y, Cons1(3, Cons1(4, _)))) => x + y
    case Cons1(h, t) => h + sum(t)
    case _ => 101
  }

  val z = List1(1, 2, 3, 4, 5)

  val y = filter(z)((x) => x %2 == 0)


  println(x)
  println(y)
}
