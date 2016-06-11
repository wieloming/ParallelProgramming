import scala.collection.GenSeq
import scala.collection.concurrent.TrieMap
import Utils.Utils._

def initialize[A](array: Array[A])(el: A) = {
  for (i <- array.indices.par) {
    array(i) = el
  }
  array
}

initialize(Array.ofDim[String](10))("pac")

List(1, 2, 3)
  .par
  .aggregate("pac")((acc, el) => acc + el, (a, b) => a + b)

////////////////////////////////////////////////////

def largestPalindrome(xs: GenSeq[Int]): Int = {
  xs.aggregate(Int.MinValue)(
    (largest, el) =>
      if (el > largest && el.toString == el.toString.reverse) el
      else largest,
    math.max
  )
}

val array = (0 until 100000).toArray
largestPalindrome(array)
largestPalindrome(array.par)

//////////////////////////////////////////////////////

val elements = (0 until 100000).map(i => (i, i + 1))
val graph = TrieMap[Int, Int]() ++= elements
graph(graph.size - 1) = 0
val previous = graph.snapshot()
for ((k, v) <- graph.par) graph(k) = previous(v)
val violation = graph.find({ case (i, v) => v != (i + 2) % graph.size })

////////////////////////////////////////////////////
trait Iterator[A] {
  def next(): A
  def hasNext: Boolean
  def foldLeft[B](acc: B)(f: (B, A) => B): B = {
    var sum = acc
    while (hasNext) sum = f(sum, next())
    sum
  }
}

trait Splitter[A] extends Iterator[A] {
  def threshold: Int
  def split: Seq[Splitter[A]]
  def remaining: Int
  def fold(acc: A)(f: (A, A) => A): A = {
    if (remaining < threshold) foldLeft(acc)(f)
    else {
      val children = for (child <- split) yield task(child.fold(acc)(f))
      children.map(_.join()).foldLeft(acc)(f)
    }
  }
}



