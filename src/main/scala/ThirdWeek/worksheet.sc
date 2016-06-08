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
    (largest, n) =>
      if (n > largest && n.toString == n.toString.reverse) n
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
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var s = z
    while (hasNext) s = f(s, next())
    s
  }
}

trait Splitter[A] extends Iterator[A] {
  def threshold: Int
  def split: Seq[Splitter[A]]
  def remaining: Int
  def fold(z: A)(f: (A, A) => A): A = {
    if (remaining < threshold) foldLeft(z)(f)
    else {
      val children = for (child <- split) yield task { child.fold(z)(f) }
      children.map(_.join()).foldLeft(z)(f)
    }
  }
}



