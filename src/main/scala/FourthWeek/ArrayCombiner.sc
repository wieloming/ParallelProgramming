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
