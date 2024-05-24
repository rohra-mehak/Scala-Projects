
import scala.math.{exp, pow}
import scala.util.Random
object Chernoff {
  val n: Int = 600
  val numTrials = 1000
  val p = 1.0 / 6.0 // Probability of success for each Bernoulli variable
  val delta = 1.0 / 5.0 // Desired level of confidence
  val mu = n * p // Expected value of the sum of variables (n * p)


  def main(args: Array[String]): Unit = {
    val chernoffBoundvalue = chernoffBound()
    val probability = probability_calc()
    println(s"Chernoff Bound: $chernoffBoundvalue")
    println(s"Probability: $probability")
    if (probability <= chernoffBound)
      println("Chernoff bound is satisfied")
  }


  def chernoffBound(): Double = {
    val exponent = -0.5 * pow(delta, 2) * mu
    val bound = exp(exponent)
    bound
  }

  def probability_calc() = {
    val verificationProbability = (1 to numTrials).foldLeft(0) { (count, _) =>
      val v=List.fill(n)(math.floorMod(Random.nextInt(), 6))
      val a = math.floorMod(Random.nextInt(), 6)
      val sumX = v.foldLeft(0) { (acc, i) =>
        // the solutions from both ways of calculating xi are comparable
//        val xi = (if i == a)1 else 0
        val xi = if (i < p) 1 else 0
        acc + xi
      }
      val lowerBound = (1 - delta) * mu
      if (sumX <= lowerBound) count + 1
      else count
    }
    verificationProbability.toDouble / 1000
  }
}