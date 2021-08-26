package part4

import cats.Eval

object SaferFoldingUsingEval extends App {
  def foldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRight(tail, acc)(fn)))
      case Nil => acc
    }

  println(foldRight((1 to 100000).toList, Eval.now(0L))((x,y) => y.map(_ + x)).value)
}
