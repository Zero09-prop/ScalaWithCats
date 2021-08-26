package part3

trait Printable[A] {
  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    (value: B) => Printable.this.format(func(value))

}
final case class Box[A](value: A)

object Impl {
  implicit val stringPrintable: Printable[String] =
    (value: String) => s"'$value'"

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  implicit def boxPrintable[A](implicit print: Printable[A]): Printable[Box[A]] = print.contramap[Box[A]](_.value)
}

object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
}
object ShowingOffWithContramap extends App {
  import Impl._

  Printable.format("hello")
  // res2: String = "'hello'"
  println(Printable.format("hello"))

  Printable.format(true)
  // res3: String = "yes"
  println(Printable.format(true))

  println(Printable.format(Box("Hello")))

  println(Printable.format(Box(false)))

  //println(Printable.format(Box(123)))
}
