package part1

import cats._
import cats.implicits._

object EqImplicit {
  implicit val eqCat: Eq[Cat] = (x: Cat, y: Cat) => x.age == y.age && x.name == y.name && x.color == y.color
}

object EqualityLibertyFelinity extends App {
  import EqImplicit._
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(s"cat1 === cat2 = ${cat1 === cat2}")
  println(s"cat1 =!= cat2 = ${cat1 =!= cat2}")
  println(s"optionCat1 === optionCat2 = ${optionCat1 === optionCat2}")
  println(s"optionCat1 =!= optionCat2 = ${optionCat1 =!= optionCat2}")
}
