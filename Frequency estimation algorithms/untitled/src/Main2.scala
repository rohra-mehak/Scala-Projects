import scala.util.Random

object Main2 {
  def main(args: Array[String]): Unit = {

    val m: Int = Math.pow(2, 20).toInt + 1
    val random: Random = new Random()

    // Generate random numbers
    val numbers: Seq[Double] = (1 to m).map(_ => random.nextDouble())

    // Compute arithmetic mean
    val arithmeticMean: Double = numbers.sum / m

    // Compute quadratic mean
    val quadraticMean: Double = math.sqrt(numbers.map(n => n * n).sum / m)

    // Compute cubic mean
    val cubicMean: Double = math.cbrt(numbers.map(n => n * n * n).sum / m)


    val sortedNumbers = numbers.sorted
    val median = if (sortedNumbers.length % 2 == 0) {
      val mid = sortedNumbers.length / 2
      (sortedNumbers(mid - 1) + sortedNumbers(mid)) / 2.0
    } else {
      sortedNumbers(sortedNumbers.length / 2).toDouble
    }

    println(s"Median: $median")
    println(s"Arithmetic Mean: $arithmeticMean")
    println(s"Quadratic Mean: $quadraticMean")
    println(s"Cubic Mean: $cubicMean")

    // Check generalized mean inequalities
    val meanInequality1 = arithmeticMean <= quadraticMean
    val meanInequality2 = quadraticMean <= cubicMean

    println(s"Generalized Mean Inequality (M1 <= M2): $meanInequality1")
    println(s"Generalized Mean Inequality (M2 <= M3): $meanInequality2")
  }
}
