

object Main {
  import scala.io._
  import scala.util.Random
  def main(args: Array[String]): Unit = {

    //task 1
    val range = 2 to 100
    val k = for (i <- range if i%2 !=0; if i%3 !=0; if i%5 !=0) yield(i * i)

    val s = range.filter(i => i % 2 != 0 && i % 3 != 0 && i % 5 != 0)
      .map(i => i * i)
    println(s)
    println(k)

    val d = for (i <- range if i%2 !=0; j <- range if j%2==0) yield(i, j)
    println(d)
    val a = range.filter(i => i % 2 != 0)
      .flatMap(i => range.filter(j => j % 2 == 0)
        .map(j => (i, j)))
    println(a)
    println(d==a)


    //task 3
    val text = "alice29.txt"
    val fileSource = Source.fromFile(text)
    val words = fileSource.mkString.split("\\W+")

//    val counts = words.groupBy(identity).view.mapValues(_.length).foreach(println)
//    println(counts)
//    val counts = words.groupBy(identity).view.mapValues(_.length)
//
//    val moment0 = counts.values.sum.toDouble
//    val moment1 = counts.values.map(math.pow(_, 1)).sum
//    val moment2 = counts.values.map(math.pow(_, 2)).sum
//    val moment3 = counts.values.map(math.pow(_, 3)).sum
//
//    println(s"0th moment: $moment0")
//    println(s"1st moment: $moment1")
//    println(s"2nd moment: $moment2")
//    println(s"3rd moment: $moment3")

//    fileSource.close

//    val words = fileSource.mkString.split("\\W+")
    val wordCounts = words.groupBy(identity).view.mapValues(_.length)

    val moment0 = wordCounts.values.map(math.pow(_, 0)).sum
    val m0 = wordCounts.size
    val moment1 = wordCounts.values.map(math.pow(_, 1)).sum
    val moment2 = wordCounts.values.map(math.pow(_, 2)).sum
    val moment3 = wordCounts.values.map(math.pow(_, 3)).sum

    println(s"0th moment: $moment0")
    println(s"0th moment: $m0")
    println(s"1st moment: $moment1")
    println(s"2nd moment: $moment2")
    println(s"3rd moment: $moment3")

    val moments = (0 to 3).map(k => computeMoment(words, k))
    // Compute the moments from 0 to 3
    println(moments)

    val minmaxfor2ndMoment = minMaxSecondMoment(2962.0 ,27334.0 )
    println(minmaxfor2ndMoment)
    // the actual second moment
    println(s"2nd moment: $moment2")

    //In the context of the text (stream) analysis,
    // the 0th moment represents the total number of words in the text,
    // while the 1st moment represents the sum of the occurrences of each word
    // in the text.

    val n = 50000
    val result = stochasticCounting(n)
    println(s"The maximal possible rho for  random numbers is: $result")
    println(n , math.pow(2, result))

    val xs = (1 to n).map(_ => Random.nextInt())
    val vs = xs.toList
    val m = stochasticCounting(vs)
    println(s"Bucket value: $m")
    println(n , math.pow(2, m))


//    val maxRho = computeMaxRho(n)
//    println(s"The maximal possible rho for $n random numbers is: $maxRho")
//    println(math.pow(2, maxRho))

  }


  def computeMoment(words: Array[String], k: Int): Long =
    words.groupMapReduce(identity)(_ => 1L)(_ + _).values.map(Math.pow(_, k)).sum.toLong


  //task 5
  def minMaxSecondMoment(n0: Double, n1: Double): (Double, Double) = {
    val mu1 = n1 / n0 // first moment
    val a = n0 // lower bound for second moment
    val b = n1 * mu1 // upper bound for second moment
    (a, b)
  }

  //task 6
  def computeMaxRho(n: Int): Int = {
    var m = 0
    for (i <- 1 to n) {
      val x = Random.nextInt()
      val rhoX = Integer.numberOfLeadingZeros(x) + 1
      if (rhoX > m) {
        m = rhoX
      }
    }
    m
  }

  def stochasticCounting(n: Int): Int = {
    def rho(x: Int): Int = Integer.numberOfLeadingZeros(x) + 1

    val xs = (1 to n).map(_ => Random.nextInt())
    val M = xs.map(rho).foldLeft(0)(math.max)
    M
  }

  def stochasticCounting(xs: List[Int], m: Int = 0): Int = xs match {
    case Nil => m
    case x :: tail =>
      val rho = Integer.numberOfLeadingZeros(x) + 1
      stochasticCounting(tail, if (rho > m) rho else m)
  }




}

