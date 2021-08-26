package part3

import cats._
import cats.implicits._

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object FunctorImplicits {
  implicit val functorTree: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(v)      => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }
  }
}

object Tree{
  def leaf[A](value: A) : Tree[A] = Leaf(value)
  def branch[A](left: Tree[A],right: Tree[A]) : Tree[A] = Branch(left,right)
}
object BranchingOutWithFunctors extends App {
  import FunctorImplicits._
  val tree: Tree[Double] = Tree.branch(Leaf(1),Leaf(2))
  val tree2 = tree.map(_ * 2).map( _ / 10).map(x => s"Hello its $x")
  println(tree2)

}
