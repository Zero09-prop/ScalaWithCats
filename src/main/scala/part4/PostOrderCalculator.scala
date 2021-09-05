package part4

import cats.data._

object PostOrderCalculator {
  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] = {
    State[List[Int], Int] { oldStack =>
      val newStack: List[Int] = sym match {
        case "+" => (oldStack.head + oldStack.tail.head) +: oldStack.tail.tail
        case "-" => (oldStack.head - oldStack.tail.head) +: oldStack.tail.tail
        case "/" => (oldStack.head / oldStack.tail.head) +: oldStack.tail.tail
        case "*" => (oldStack.head * oldStack.tail.head) +: oldStack.tail.tail
        case num => num.toInt +: oldStack
      }
      (newStack, newStack.head)
    }
  }
  def evalAll(input: List[String]): CalcState[Int] = {
    input.map(z => evalOne(z)).reduce((x, y) => x.flatMap(_ => y.map(a => a)))
  }
  def evalInput(input: String): CalcState[Int] = {
    evalAll(input.split("\\s").toList)
  }

  def main(args: Array[String]): Unit = {
    val program = evalOne("4")
      .flatMap(_ =>
        evalOne("3")
          .flatMap(_ =>
            evalOne("+")
              .map(a => a)
          )
      )
    println(program.runA(Nil).value)
    println(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)
    println(evalInput("1 2 + 3 *").runA(Nil).value)
  }

}
