package cstudy

import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._

object DataValidation {
  sealed trait Predicate[E, A] {
    import Predicate._
    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
    def apply(value: A)(implicit sem: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(value)
        case And(l, r) =>
          (l(value), r(value)).mapN((_, _) => value)
        case Or(l, r) =>
          l(value) match {
            case Valid(_) => Valid(value)
            case Invalid(e1) =>
              r(value) match {
                case Valid(_)    => Valid(value)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }

          }

      }
  }

  object Predicate {
    final case class And[E, A](
        left: Predicate[E, A],
        right: Predicate[E, A]
    ) extends Predicate[E, A]
    final case class Or[E, A](
        left: Predicate[E, A],
        right: Predicate[E, A]
    ) extends Predicate[E, A]
    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
    def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
  }
  sealed trait Check[E, A, B] {
    import Check._
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]
    def map[C](func: B => C): Check[E, A, C] = Map[E, A, B, C](this, func)
    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)
    def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
      AndThen[E, A, B, C](this, that)
  }
  object Check {
    final case class Map[E, A, B, C](
        check: Check[E, A, B],
        f: B => C
    ) extends Check[E, A, C] {
      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(f)
    }
    final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }
    final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
        check1(a).withEither(_.flatMap(b => check2(b).toEither))
    }
    final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {

      def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
        pred(in)
    }

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
      Pure(pred)
  }
  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(value: A): Either[E, A] = func(value)
    def and(that: CheckF[E, A])(implicit sem: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), _)         => e.asLeft
          case (_, Left(e))         => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }

      }
  }
  def main(args: Array[String]): Unit = {

  }
}
