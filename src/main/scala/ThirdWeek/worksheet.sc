import scala.collection.GenSeq

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



