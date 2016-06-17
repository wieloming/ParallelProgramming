import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import Utils.Utils._

class ArrayCombiner[T <: AnyRef : ClassTag](val parallelism: Int) {
  private var numElems = 0
  private val buffers = new ArrayBuffer[ArrayBuffer[T]]
  buffers += new ArrayBuffer[T]

  def +=(x: T) = {
    buffers.last += x
    numElems += 1
    this
  }

  def combine(that: ArrayCombiner[T]) = {
    buffers ++= that.buffers
    numElems += that.numElems
    this
  }

  def copyTo(array: Array[T], from: Int, end: Int) = ???

  def result: Array[T] = {
    val array = new Array[T](numElems)
    val step = math.max(1, numElems / parallelism)
    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)
    val tasks = for ((from, end) <- chunks) yield task {
      copyTo(array, from, end)
    }
    tasks.foreach(_.join())
    array
  }
}

//xs.par.aggregate(newCombiner)(_ += _, _ combine _).result

//////////////////////////////////////////////////////////
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

///////////////////////

sealed trait Conc[+T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
}
case object EmptyC extends Conc[Nothing] {
  def level = 0
  def size = 0
  def left = null
  def right = null
}
class SingleC[T](val x: T) extends Conc[T] {
  def level = 0
  def size = 1
}
case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}