import Utils.Utils._

sealed trait Tree[+T]
case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
case class Leaf[T](elem: T) extends Tree[T]
case object Empty extends Tree[Nothing]

def filter[T](t: Tree[T])(p: T => Boolean): Tree[T] = t match {
  case Node(left, right) =>
    val (r, l) = parallel(filter(left)(p), filter(right)(p))
    Node(r, l)
  case Leaf(elem) => if (p(elem)) t else Empty
  case Empty => Empty
}