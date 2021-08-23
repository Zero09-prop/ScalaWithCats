package part1

import cats._
import cats.implicits._

object CatsImplicit {
  implicit val catShow: Show[Cat] = (c: Cat) => s"${c.name} is a ${c.age} year-old ${c.color} cat"
}

object MainPrintableCats extends App {
  import CatsImplicit._

  val cat = Cat("Semen", 14, "Red")
  println(cat.show)

}
