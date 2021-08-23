package introduction

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val printableString: Printable[String] = (value: String) => value.toUpperCase
  implicit val printableInt: Printable[Int] = (value: Int) => value.toString
  implicit val printableCat: Printable[Cat] = (c: Cat) =>
    s"${printableString.format(c.name)} is a ${Printable.format(c.age)} year-old ${printableString.format(c.color)} cat"
}
object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}
object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)
  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}

final case class Cat(name: String, age: Int, color: String)

object MainPrintable extends App {
  import PrintableInstances._
  import PrintableSyntax._

  val cat = Cat("Semen", 14, "Red")
  cat.print
  println(cat.format)
}
