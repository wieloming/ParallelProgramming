

///////////////////////

sealed trait Conc[T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
  def normalized = this
}

case class <>[+T](left: Conc[T], right: Conc[T]) extends Conc[T] {
  val level = 1 + math.max(left.level, right.level)
  val size = left.size + right.size
}

object Conc {

  sealed trait Leaf[T] extends Conc[T] {
    def left = throw new UnsupportedOperationException
    def right = throw new UnsupportedOperationException
  }

  case object Empty extends Leaf[Nothing] {
    def level = 0
    def size = 0
  }
  class Single[T](val x: T) extends Leaf[T] {
    def level = 0
    def size = 1
    override def toString = s"Single($x)"
  }
  class Chunk[T](val array: Array[T], val size: Int, val k: Int) extends Leaf[T] {
    def level = 0
    override def toString = s"Chunk(${array.mkString("", ", ", "")}; $size; $k)"
  }
  case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level = 1 + math.max(left.level, right.level)
    val size = left.size + right.size
  }
}

