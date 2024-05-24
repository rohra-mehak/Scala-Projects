import scala.util.Random

object BienaymeChebyshevInequality {
  def main(args: Array[String]): Unit = {
    val n: Int = 1000000
    val k: Double = 3.0
    val runs: Int = 1000

    val random = new Random()

    val probabilities = (1 to runs).map { _ =>
      val v = List.fill(n)(random.nextGaussian())
      val mean: Double = v.sum / n
      val squaredValues = v.map(x => x * x)
      val meanOfSquaredValues: Double = squaredValues.sum / n
      val variance: Double = meanOfSquaredValues - mean * mean
      val sigma: Double = math.sqrt(variance)
      val probability: Double = v.count(x => math.abs(x - mean) >= k * sigma).toDouble / n

      probability
    }

    val probability: Double = probabilities.sum / runs
    println(s"probability: $probability")
    if (probability <=1 /(k*k)) {
      println("Bienayme–Chebyshev inequality is satisfied.")
    } else {
      println("Bienayme–Chebyshev inequality is not satisfied.")
    }
  }
}
