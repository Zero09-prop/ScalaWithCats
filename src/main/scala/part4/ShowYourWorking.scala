package part4

import cats.data.Writer
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

object ShowYourWorking {
  def slowly[A](body: => A): A =
    try body
    finally Thread.sleep(100)

  type Fact = Writer[Vector[String], Int]

  def factorial(n: Int): Fact = {
    val ans: Fact = slowly(if (n == 0) 1.writer(Vector()) else factorial(n - 1).map(_ * n))
    ans.mapWritten(x => x :+ s"fact $n ${ans.value}")
  }
  def main(args: Array[String]): Unit = {
    println(factorial(5))

    val f1 = Future(factorial(10))
    val f2 = Future(factorial(15))

    Thread.sleep(10 * 1000)

    println(f1)
    println(f2)
  }
}
