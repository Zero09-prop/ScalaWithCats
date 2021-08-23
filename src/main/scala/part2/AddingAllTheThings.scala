package part2

import cats._
import cats.implicits._

final case class Order(totalCost: Double, quantity: Double)

object MonoImplicit {
  implicit val monoidOrder: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
  }
}
object AddingAllTheThings extends App {
  import MonoImplicit._

  def add(items: List[Int]): Int = {
    items.fold(0)((x, y) => x |+| y)
  }
  def superAdder(items: List[Option[Int]]): Option[Int] = items.fold(Monoid[Option[Int]].empty)(_ |+| _)
  def superSuperAdder(items: List[Order]): Order = items.fold(Monoid[Order].empty)(_ |+| _)

  val or1 = Order(25.8, 3)
  val or2 = Order(78.3, 1)
  println(add(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(superAdder(List(Option(1), Option(3), Option(0))))
  println(superSuperAdder(List(or1, or2)))

}
