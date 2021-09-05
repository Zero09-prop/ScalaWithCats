package part4

import cats._
import cats.implicits._

import scala.annotation.tailrec

object BranchingOutFurtherWithMonads {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = leaf(a)

    override def flatMap[A, B](value: Tree[A])(func: A => Tree[B]): Tree[B] =
      value match {
        case Branch(l, r) => branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(v)      => func(v)
      }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Right(value) => leaf(value)
        case Left(value)  => tailRecM(value)(f)

      }
  }
  def main(args: Array[String]): Unit = {
    val tree = branch(leaf(3), leaf(4))
    val a = treeMonad.flatMap(tree)(a => branch(leaf(a * 2), leaf(3 * a)))
    println(s"a = ${a}")
  }
}
