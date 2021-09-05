package part7

object ReflectingOnFolds {
  def main(args: Array[String]): Unit = {
    println(List(1, 2, 3).foldLeft(List.empty[Int])((a, z) => z :: a))
    println(List(1, 2, 3).foldRight(List.empty[Int])((z, a) => z :: a))
  }
}
