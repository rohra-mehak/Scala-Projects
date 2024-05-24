
import scala.collection.mutable
import scala.util.hashing.MurmurHash3
import scala.util.hashing.MurmurHash3
import scala.io._

object Main {
  def main(args: Array[String]): Unit = {
    val f = (i: Int) => Option(i * i)
    val g = (i: Int) => Option(i * i * i)

//     Left unit
    val x = 2
    val left = Some(x).flatMap(f) == f(x)
    val right = f(x) == Some(x * x)
    println(left == right)

//    Right unit
    val m: Option[Int] = Some(2)
    val l = m.flatMap(Some.apply) == m
    val r = m == Some(2)
    println(l == r)

// Associativity
    val y: Option[Int] = Some(2)
    val lhs = y.flatMap(f).flatMap(g) == y.flatMap(x => f(x).flatMap(g))
    val rhs = Some(2 * 2 * 2 * 2 * 2 * 2) == f(2).flatMap(g)
    println(lhs == rhs)

    val xx = 1729
    val yy = Math.pow(xx, 2).toInt
    println(yy)
    val distance = hammingDistance(xx, yy)
    println(s"The Hamming distance between $xx and $yy is $distance")

//    val text = "alice29.txt"
      val text = "bible.txt"


    val fileSource = Source.fromFile(text)
    val words = fileSource.mkString.split("\\W+").map(x => x.toLowerCase())
//    println(words)
    val length = words.distinct.length


   val filter = new BloomFilter(10000, 5)

    // Add words to the filter
    val updatedFilter = words.foldLeft(filter) { (acc, word) =>
      acc.add(word)
      acc
    }

    // Count occurrences of words
    val counts = words.foldLeft(mutable.Map.empty[String, Int]) { (acc, word) =>
      if (updatedFilter.contains(word)) {
        acc(word) = acc.getOrElse(word, 0) + 1
      }
      acc
    }

    // Print k most frequent words
    val k = 30
    val topWords = counts.toSeq.sortBy(-_._2).take(k)
    println(s"Top $k words: ${topWords.mkString(", ")}")

    // Print false positives
    val falsePositives = words.filterNot(updatedFilter.contains)
    println(s"False positives: ${falsePositives.mkString(", ")}")

    // Print actual and theoretical false positive rates
    val actualSize = counts.size
    val theoreticalFPR = updatedFilter.falsePositiveRate(actualSize)
    val actualFPR = falsePositives.length.toDouble / words.length
    println(s"Actual false positive rate: $actualFPR")
    println(s"Theoretical false positive rate: $theoreticalFPR")

    val filter_2 = new BloomFilter(16 * 1024, 6)
    val uFilter = words.foldLeft(filter_2) { (acc, word) =>
      acc.add(word)
      acc
    }
    val distinctCount = uFilter.distinctCount
    println(s"Estimated distinct count: $distinctCount")
    println(s"distinct count: $length")


    val S = List('a', 'a', 'a', 'c', 'c', 'b', 'b', 'c', 'c', 'c', 'b', 'c', 'c')
    var count = 1
    println(s"Step $count: T = None, n = 0")
    count += 1

    majority(S) match {
      case Some(majority) => println(s"Step $count: T = $majority, n = ${S.count(_ == majority)}")
      case None => println("No majority element")
    }


  }

  def hammingDistance(x: Int, y: Int): Int = {
    val xorResult = x ^ y

    (0 until 32).foldLeft(0)((acc, i) => {
      if (((xorResult >> i) & 1) == 1) {
        acc + 1
      } else {
        acc
      }
    })
  }

  def findMostCommonWords(words: List[String], k: Int): List[String] = {
    val bloomFilter = mutable.BitSet.empty
    val counts = words.foldLeft(Map.empty[String, Int]) { (acc, word) =>
      val hash = MurmurHash3.stringHash(word)
      if (!bloomFilter.contains(hash)) {
        bloomFilter.add(hash)
        acc + (word -> (acc.getOrElse(word, 0) + 1))
      } else {
        acc + (word -> (acc(word) + 1))
      }
    }

    counts.toList.sortWith((a, b) => a._2 > b._2).take(k).map(_._1)
  }


  def majority(elements: List[Char]): Option[Char] = {
    def majorityHelper(elements: List[Char], registry: Option[Char], count: Int): Option[Char] = {
      elements match {
        case Nil => registry
        case head :: tail =>
          if (count == 0) {
            majorityHelper(tail, Some(head), 1)
          } else if (Some(head) == registry) {
            majorityHelper(tail, registry, count + 1)
          } else {
            majorityHelper(tail, registry, count - 1)
          }
      }
    }

    majorityHelper(elements, None, 0)
  }


}