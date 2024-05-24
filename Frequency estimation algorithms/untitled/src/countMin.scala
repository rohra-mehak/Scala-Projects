import scala.io.Source
import scala.util.hashing.MurmurHash3
import scala.collection.mutable
object countMin {
  private def hash(word: String, seed: Int): Int = {
    Math.abs(MurmurHash3.stringHash(word, seed))
  }

  def countMin(w: Int, d: Int, words: List[String]): mutable.HashMap[String, Int] = {

    val arr: Array[Array[Int]] = (0 until d).foldLeft(Array.ofDim[Int](w, d)) { (acc, i) =>
      words.foldLeft(acc) { (arr, word) =>
        val x = hash(word, i)
        arr.updated(x % w, arr(x % w).updated(i, arr(x % w)(i) + 1))
      }
    }

    val freq: mutable.HashMap[String, Int] = words.foldLeft(mutable.HashMap.empty[String, Int]) { (acc, word) =>
      val minCount = (0 until d).foldLeft(Int.MaxValue) { (minCount, i) =>
        val x = hash(word, i)
        val m = arr(x % w)(i)
        Math.min(minCount, m)
      }
      acc + (word -> minCount)
    }

    freq
  }


  def main(args: Array[String]): Unit = {
    val w = Math.pow(2, 16).toInt // Number of buckets
    val d = 4 // Number of hash functions

    val text_file = Source.fromFile("alice29.txt")
    val alice = text_file.mkString.toLowerCase().split("\\W+").toList
    text_file.close()
    val aliceCount = countMin(w,d,alice)
    val actualFrequencies = alice.groupMapReduce(identity)(_ => 1)(_ + _)

    val differencesCount = aliceCount.foldLeft(0) { case (count, (key, value1)) =>
      val value2 = actualFrequencies.getOrElse(key, 0)
      if (value1 != value2) count + 1 else count
    }

    println(s"Count of wrongly estimated words in Alice : $differencesCount")


    val text2_file = Source.fromFile("bible.txt")
    val bible = text2_file.mkString.toLowerCase().split("\\W+").toList
    text2_file.close()

    val bibCount = countMin(w,d,bible)

    val actualFrequencies2 = bible.groupMapReduce(identity)(_ => 1)(_ + _)
    val differencesCount2 = bibCount.foldLeft(0) { case (count, (key, value1)) =>
      val value2 = actualFrequencies2.getOrElse(key, 0)
      if (value1 != value2) count + 1 else count
    }

    println(s"Count of wrongly estimated words in Bible : $differencesCount2")
  }

}
