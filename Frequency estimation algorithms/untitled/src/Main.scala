import scala.collection.mutable
import scala.io._
object Main {
  def main(args: Array[String]): Unit = {

    val filename = "alice29.txt"
    val source = scala.io.Source.fromFile(filename)
    val stream = source.mkString.split("\\W+").map(x => x.toLowerCase()).filter(_.nonEmpty).toList
    val k = 21
    val m = stream.length
    val misraGries = new MisraGries(stream,k, m)
    misraGries.processStream()
    val  threshold = (m /k).toDouble
    println(s"frequency threshold $threshold \n")
    println("%15s | %15s | %15s | %15s | %15s | %15s".format("Word", "Frequency", "Lower Bound", "Misra-Gries Freq", "False Positive", "Estimate Holds"))
    misraGries.summarizeWords()

  }

}