package part2

object MonoidImplSet {
  implicit def monoidUnion[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]

      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
    }
//  implicit def semiSymDif[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
//    override def empty: Set[A] = Set.empty[A]
//
//    override def combine(x: Set[A], y: Set[A]): Set[A] =  (x diff y) union (y diff x)
//  }
}
object SemigroupImpl {
  implicit def semi[A]: Semigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => x intersect y
//  implicit def semiSymDif[A]: Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => (x diff y) union (y diff x)
}
object AllSetMonoid extends App {
  import MonoidImplSet._
  //import SemigroupImpl._

  val mono1 = Monoid[Set[Int]]
  val mono2 = Monoid[Set[String]]
  val sem = Semigroup[Set[Int]]
  println(sem.combine(Set(1, 3), Set(3, 4)))
}
