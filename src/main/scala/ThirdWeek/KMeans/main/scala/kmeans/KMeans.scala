package ThirdWeek.KMeans.main.scala.kmeans

import org.scalameter._

import scala.annotation.tailrec
import scala.collection._
import scala.language.postfixOps
import scala.util.Random

class KMeans {

  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val randX = new Random(1)
    val randY = new Random(3)
    val randZ = new Random(5)
    (0 until num)
      .map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randX.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randY.nextDouble() * 0.5
        val z = ((i + 7) % k) * 1.0 / k + randZ.nextDouble() * 0.5
        new Point(x, y, z)
      }).to[mutable.ArrayBuffer]
  }

  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = new Random(7)
    (0 until k).map(_ => points(rand.nextInt(points.length))).to[mutable.ArrayBuffer]
  }

  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    assert(means.nonEmpty)
    var minDistance = p.squareDistance(means.head)
    var closest = means.head
    var i = 1
    while (i < means.length) {
      val distance = p.squareDistance(means(i))
      if (distance < minDistance) {
        minDistance = distance
        closest = means(i)
      }
      i += 1
    }
    closest
  }

  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenMap[Point, GenSeq[Point]] = {
    val grouped = points.groupBy(findClosest(_, means))
    grouped ++ means.filterNot(grouped contains).map((_, GenSeq()))
  }

  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = if (points.isEmpty) oldMean
  else {
    var x = 0.0
    var y = 0.0
    var z = 0.0
    points.seq.foreach { p =>
      x += p.x
      y += p.y
      z += p.z
    }
    new Point(x / points.length, y / points.length, z / points.length)
  }

  def update(classified: GenMap[Point, GenSeq[Point]], oldMeans: GenSeq[Point]): GenSeq[Point] = {
    oldMeans map (point => findAverage(point, classified(point)))
  }

  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    val pairs = oldMeans zip newMeans
    val difference = pairs.foldLeft(0.0) { case (acc, (oldM, newM)) => acc + oldM.squareDistance(newM) }
    difference < eta
  }

  @tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val classification = classify(points, means)
    val newMeans = update(classification, means)
    if (converged(eta)(means, newMeans)) kMeans(points, newMeans, eta)
    else newMeans
  }
}

/** Describes one point in three-dimensional space.
  *
  * Note: deliberately uses reference equality.
  */
class Point(val x: Double, val y: Double, val z: Double) {
  private def square(v: Double): Double = v * v
  def squareDistance(that: Point): Double = square(that.x - x) + square(that.y - y) + square(that.z - z)
  private def round(v: Double): Double = (v * 100).toInt / 100.0
  override def toString = s"(${round(x)}, ${round(y)}, ${round(z)})"
}


object KMeansRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]) {
    val kMeans = new KMeans()

    val numPoints = 500000
    val eta = 0.01
    val k = 32
    val points = kMeans.generatePoints(k, numPoints)
    val means = kMeans.initializeMeans(k, points)

    val seqTime = standardConfig measure {
      kMeans.kMeans(points, means, eta)
    }
    println(s"sequential time: $seqTime ms")

    val parTime = standardConfig measure {
      val parPoints = points.par
      val parMeans = means.par
      kMeans.kMeans(parPoints, parMeans, eta)
    }
    println(s"parallel time: $parTime ms")
    println(s"speedup: ${seqTime / parTime}")
  }

}
