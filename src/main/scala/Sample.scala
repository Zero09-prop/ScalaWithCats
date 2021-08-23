import cats._
import cats.implicits._
final case class Cat(name: String, age: Int, color: String)

object Sample extends App {
  implicit val monoidCat: Monoid[Cat] = new Monoid[Cat]{
    override def empty: Cat = Cat("",0,"")

    override def combine(x: Cat, y: Cat): Cat = Cat(x.name + y.name,x.age + y.age,x.color + y.color)
  }
  val cat1 = Cat("semen",1,"red")
  val cat2 = Cat("Garf",3,"orange")
  val s: String = "123"
  println(cat1 |+| cat2)
}
