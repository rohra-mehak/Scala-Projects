import scala.util.Random

object MedianProbability {
  val mu = 0.5
  val p = 1.0 / 6.0
  val runs = 1000
  val s =600

  def main(args: Array[String]): Unit = {
    val (count, countZ) = (1 to runs).foldLeft(0, 0) { case ((countAcc, countZAcc), _) =>
      val Y= List.fill(s)(Random.nextDouble())
      val median = Y.sorted.apply(s / 2)
      val newCount = if (Math.abs(median - mu) > p) countAcc + 1 else countAcc
      val Z = Y.count(Yi => Math.abs(Yi - mu) <= p)
      val newCountZ = if (Z < s / 2) countZAcc + 1 else countZAcc
      (newCount, newCountZ)
    }

    val a1 =count.toDouble / runs.toDouble
    val a2 = countZ.toDouble / runs.toDouble

    printf("P(|Y - 𝜇| > 1/6) = %.9f\n", a1)
    printf("P(Z < s/2) = %.4f\n",a2 )
    if(a1 <= a2)
      println("inequality P(|Y - 𝜇| > 1/6) <=  P(Z < s/2) is satisfied")
  }
}