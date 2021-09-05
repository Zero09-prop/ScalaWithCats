package cstudy

import cats.{Foldable, Monoid}
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
object MapReduce {
  def foldMap[A, B: Monoid](vec: Vector[A])(f: A => B): B = {
    vec.map(f).foldLeft(Monoid[B].empty)((ac, b) => ac |+| b)
  }
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numProccessors = Runtime.getRuntime.availableProcessors()
    val futures = values.grouped(numProccessors) map { group =>
      Future(foldMap(group)(f))
    }
    Future.sequence(futures).map(it => it.foldLeft(Monoid.empty)(_ |+| _))
  }
  def parallelFoldMap2[A, B: Monoid](vec: Vector[A])(f: A => B): Future[B] = {
    val numProccessors = Runtime.getRuntime.availableProcessors()
    vec
      .grouped(numProccessors)
      .toVector
      .traverse(group => Future(group.foldMap(f)))
      .map(_.combineAll)

  }

  def main(args: Array[String]): Unit = {
    val vec = Vector(1, 2, 3)
    val a = (1 to 10000).toVector
    val b = parallelFoldMap(a)(_ * 2)
    println(b)
    println(foldMap(vec)(identity))
    println(foldMap(vec)(_.toString + "! "))
    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase + " "))
    Await.result(b, 1.seconds)
    println(b)
  }
}
