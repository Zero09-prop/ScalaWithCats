package part5

import cats.data._
import cats.implicits._
import cats.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object TransformAndRollOut {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  def getPowerLevel(autobot: String): Response[Int] = {
    EitherT(Future(powerLevels.get(autobot).toRight("Power level unreachable")))
  }
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    val combLvl = for {
      lvl1 <- getPowerLevel(ally1)
      lvl2 <- getPowerLevel(ally2)
    } yield lvl1 + lvl2
    combLvl.map(lvl => lvl >= 15)
  }
  def tacticalReport(ally1: String,ally2: String): String = {
    val specMove = canSpecialMove(ally1,ally2).value
    val str = s"$ally1 and $ally2"
    Await.result(specMove,3.seconds) match {
      case Left(value) => s"Comms error: $value"
      case Right(true) =>s"$str are ready to roll out!"
      case Right(false) =>s"$str need a recharge"
    }

  }

  def main(args: Array[String]): Unit = {
    powerLevels.foreach { auto =>
      val a = getPowerLevel(auto._1).map(println)
      Await.result(a.value, 3.seconds)

    }
    println(s"getPowerLevel(123) = ${getPowerLevel("123")}")
    val a = tacticalReport("123","Jazz")
    Thread.sleep(3*1000)
    println(a)
    tacticalReport("Jazz", "Bumblebee")
    // res28: String = Jazz and Bumblebee need a recharge.

    tacticalReport("Bumblebee", "Hot Rod")
    // res29: String = Bumblebee and Hot Rod are ready to roll out!

    tacticalReport("Jazz", "Ironhide")
    // res30: String = Comms error: Ironhide unreachable

  }
}
