def initialize[A](array: Array[A])(el: A) = {
  for(i <- array.indices.par){
    array(i) = el
  }
  array
}

initialize(Array.ofDim[String](10))("pac")

List(1,2,3)
  .par
  .aggregate("pac")((acc, el) => acc + el, (a, b)=> a + b)