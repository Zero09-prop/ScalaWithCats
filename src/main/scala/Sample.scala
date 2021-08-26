import cats._
import cats.data._
import cats.implicits._

final case class User(username: String, password: String)
object wrapper {
  sealed trait MyError
  case class Fatal(msg: String, er: Option[Throwable]) extends MyError
  case class Warning(msg: String) extends MyError

  case class ConfigurationApp(email: String, server: String)

  implicit class MyErrorOps(msg: String) {
    def fatal(er: Throwable): MyError = Fatal(msg, Option(er))
    def warning: MyError = Warning(msg)
  }
}
object Sample extends App {
  import wrapper._
  val a: MyError = "Error".fatal(new NullPointerException)
  type Logged[A] = Writer[Vector[MyError], A]
  val writer1 = Vector("a", "b", "c").tell.flatMap(_ => Vector("d", "e", "f").tell)
  val writer2 = writer1.mapWritten(x => x.map(_.toUpperCase))
  println(writer1.run)
  println(writer2.run)
  val wrt: Logged[ConfigurationApp] = {
    ConfigurationApp("mail.ru", "localhost").writer(Vector("Error".fatal(new NullPointerException)))
  }
  val wrt2 = wrt.reset
  println(wrt.run)
  println(wrt2.run)
}
