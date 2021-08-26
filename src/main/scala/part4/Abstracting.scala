package part4

import cats._
import cats.implicits._

import scala.util.Try

object Abstracting extends App {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(age.pure[F])(new IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)

  println(validateAdult[Try](18))

  println(validateAdult[Try](8))

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))
}
