package part7
import scala.math.Numeric

object ScafFoldingOtherMethods {
  def map[A, B](lst: List[A])(f: A => B): List[B] = {
    lst.foldRight(List.empty[B])((b, ac) => f(b) :: ac)
  }
  def filter[A](lst: List[A])(p: A => Boolean): List[A] =
    lst.foldRight(List.empty[A])((b, ac) => if (p(b)) b :: ac else ac)
  def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] = {
    lst.foldRight(List.empty[B])((b,ac) => f(b) ++ ac)
  }
  def sum[A](lst: List[A])(implicit numeric: Numeric[A]): A = {
    lst.foldRight(numeric.zero)((b,ac) => numeric.plus(b,ac) )
  }
  def main(args: Array[String]): Unit = {
    println(map(List(1, 2, 3, 5))(_ * 2))
    println(filter(List(1, 2, 3, 4, 5))(el => el % 2 != 0))
    println(flatMap(List(1,2,3,4))(x => List(s"$x!")))
    println(sum(List(1,2,3)))
  }

}
