package part4

import cats._
import cats.implicits._

object MonadicSecretIdentities extends App {
  type Id2[A] = A
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  def pure[A](a: A): Id2[A] = a

  def flatMap[A, B](value: Id2[A])(func: A => Id2[B]): Id2[B] = func(value)

  def map[A, B](value: Id2[A])(func: A => B): Id2[B] = func(value)

  val a: cats.Id[String] = "hello"
  val b: Id2[String] = "hello"
  println(a == b)

  println(sumSquare(3: Id2[Int], 4: Id2[Int]))

}
