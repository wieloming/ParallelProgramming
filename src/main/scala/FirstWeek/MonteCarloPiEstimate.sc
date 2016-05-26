import scala.util.Random
import FirstWeek.Utils._


def mc(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for(i <- 0 until iter){
    val x = randomX.nextDouble()
    val y = randomX.nextDouble()
    if(x*x + y*y < 1) hits += 1
  }
  hits
}

def monteCarloPiSeq(iter: Int) =
  4.0 * mc(iter) / iter
def monteCarloPiPar(iter: Int) = {
  val ((pi1, pi2), (pi3, pi4)) =
    parallel(
      parallel(mc(iter/4), mc(iter/4)),
      parallel(mc(iter/4), mc(iter - 3*(iter/4)))
    )
  4.0 * (pi1 + pi2 + pi3 + pi4) / iter
}