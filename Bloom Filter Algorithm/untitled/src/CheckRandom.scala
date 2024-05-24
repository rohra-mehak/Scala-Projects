object CheckRandom {

  def main(args: Array[String]): Unit = {

    val p = 28 // 29 AND 30
    val n = math.pow(2, p).toInt
    val random = new scala.util.Random(42)
    val numbers = Array.fill(n)(random.nextInt())
    val hll = numbers.foldLeft(new HyperLogLog(10)) { (hll, number) =>
      hll.add(number.toString)
      hll
    }
    val E = (1 to 10).map(_ => hll.count()).sum / 10.0
    val A = analyticEstimate(n, hll.b)
    println(s"A = $A, E = $E")
  }

  def analyticEstimate(n: Int, b: Int): Double = {
    val m = math.pow(2, b).toDouble
    val x = n.toDouble / m
    if (x < 2.5) {
      val V = math.max(m - n, 0.0)
      m * math.log(m / V)
    } else if (x <= 1.0 / 30.0 * math.pow(2, 32)) {
      alpha(b) * m * m / x
    } else {
      -math.pow(2, 32) * math.log(1 - x / math.pow(2, 32))
    }
  }

  def alpha(b: Int): Double = {
    b match {
      case 4 => 0.673
      case 5 => 0.697
      case 6 => 0.709
      case _ => 0.7213 / (1 + 1.079 / math.pow(2, b))
    }
  }

}

// prints for  p = 28


