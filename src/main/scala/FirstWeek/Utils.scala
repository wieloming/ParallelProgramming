package FirstWeek

import scala.language.implicitConversions

object Utils {

  //TASKS
  trait Task[A] { def join: A}
  def task[A](c: => A): Task[A] = ???
  implicit def getJoin[T](x: Task[T]): T = x.join


  def parallel[A, B](cA: => A, cB: => B): (A, B) = {
    val taskB = task(cB)
    val taskA = cA
    (taskA, taskB.join)
  }

}
