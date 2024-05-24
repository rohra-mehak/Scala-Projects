import scala.io.Source
import scala.util.Random

object AMS {


  def alonMatias(stream: List[String], m: Int, r: Int) = {
    val random = new Random()
    var indices = (1 to r).map(_ => random.nextInt(m)).toList

    val c = indices.zipWithIndex.map { case (i, index) =>
      val word = stream(i)
      val count = stream.slice(i, m).count(_ == word)
      (index, count)
    }.toList

    val cc = c.map(_._2).sum.toDouble / c.length

        val second = m*(2*cc - 1).toDouble
        val third = m*(math.pow(cc, 3) - math.pow(cc -1 , 3)).toDouble

    println(s"Estimation of Second Moment: $second")
    println(s"Estimation of Third Moment: $third")

  }

  def main(args: Array[String]): Unit = {
    val text_file = Source.fromFile("alice29.txt")
    val alice = text_file.mkString.toLowerCase().split("\\W+").toList
    text_file.close()

    alonMatias(alice, alice.length,60)
    //
    val exactSecondMoment = alice.groupBy(l => l).map(t => Math.pow(t._2.length, 2)).sum
    val exactThirdMoment = alice.groupBy(l => l).map(t => Math.pow(t._2.length, 3)).sum
    println(s"Exact Second Moment: $exactSecondMoment")
    println(s"Exact Third Moment: $exactThirdMoment")
  }
}

