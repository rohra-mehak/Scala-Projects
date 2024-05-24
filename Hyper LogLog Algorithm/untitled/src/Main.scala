import scala.util.hashing.MurmurHash3

import scala.io._



object Main {



  def main(args: Array[String]): Unit = {

    val num: Int = 1729 * 1729

    println(num)

    val weight = hammingWeight(num)

    println(s"Hamming Weight: $weight")

    val text = "alice29.txt"

    //  val text = "bible.txt"



    val fileSource = Source.fromFile(text)

    val words = fileSource.mkString.split("\\W+")

    val length = words.distinct.length

    fileSource.close()





    val hll = new HyperLogLog(10)

    val estimate = words.foldLeft(hll)((acc, x) => {

      acc.add(x); acc

    }).count()



    println(s"Estimated distinct elements: $estimate")

    // prints  Estimated distinct elements: 2905.2182972184087



    println(s" Exact distinct elements: $length")



    // prints  Exact distinct elements: 2962







  }

  def hammingWeight(n: Int): Int = {

    println(n.toBinaryString)

    n.toBinaryString.count(_ == '1')

  }



}

