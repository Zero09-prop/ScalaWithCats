package part4

import cats._
import cats.implicits._
import cats.data._

object HackingReaders {
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )
  type DbReader[B] = Reader[Db, B]

  def findUsername(id: Int): DbReader[Option[String]] = Reader(_.usernames.get(id))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId)
      .flatMap(username =>
        username.map { x => checkPassword(x, password) }.getOrElse(false.pure[DbReader])
      )

  def main(args: Array[String]): Unit = {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )

    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    println(checkLogin(1, "zerocool").run(db))
    // res7: cats.package.Id[Boolean] = true
    println(checkLogin(4, "davinci").run(db))
    // res8: cats.package.Id[Boolean] = false
  }
}
