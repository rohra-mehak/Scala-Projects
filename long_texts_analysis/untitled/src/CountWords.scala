import scala.util.hashing.MurmurHash3

object CountWords {
  import scala.util.hashing.MurmurHash3
  import scala.io._

  def main(args: Array[String]): Unit = {


    val fileA = "alice29.txt"
    val fileSourceA = Source.fromFile(fileA)
    val s = fileSourceA.mkString("")
    val substrings = (0 to s.length -24 ).map(i => s.substring(i, i + 24)).toList
    val x : Option[String]  = substrings.find(substring => MurmurHash3.stringHash(substring) == 320915200)

    //task 4
    try {
      val result = x.get // try to retrieve the value
      println(result)
    } catch {
      case e: NoSuchElementException => println("No such string was found")
    }



      fileSourceA.close
//      fileSourceB.close

    //    var my_string = MurmurHash3.stringHash("hellow")
    //    val s= fileSourceA.mkString("")
    //    val substrings = (0 to s.length-24).map(i => s.substring(i, i + 24)).toList
    //    var x : String = ""
    //    for (x <- substrings) {
    //      if (MurmurHash3.stringHash(x) == 320915200 ){
    //        x
    //      }
    //      else println("not found")
    //    }
    //    substrings.find(substring => MurmurHash3.stringHash(substring) ==  320915200)
    //    val substrings = for {
    //      i <- 0 until wordsA.length
    //      j <- i + 1 to wordsA.length
    //      substring = wordsA.substring(i, j)
    //      if MurmurHash3.stringHash(substring) ==  320915200
    //    } yield substring
    //    println(wordsA.find(x => {MurmurHash3.stringHash(x) == 320915200}))


//       println(kMostFrequentWords(wordsA.map(x => x.toLowerCase), k))
//    println("number of  words: " + wordsA.length)

    // task 3
//     println("number of distinct words: " + wordsA.map(x => x.toLowerCase).distinct.length)
//     println("most fequently occuring word " + wordsA.groupBy(identity).view.mapValues(_.size).maxBy(x => x._2)._1)
//
//    //    println(words.map(x => x.toLowerCase) diff(words2.map(x => x.toLowerCase)))
////    println(List(words diff (words2)))
//    val wordsB = fileSourceB.mkString.split("\\W+")
//    val AnotB  = wordsA.map(x => x.toLowerCase)
//       .filterNot(x =>
//         wordsB.map(x => x.toLowerCase).contains(x))
//
//    println(AnotB.length)
//    println(AnotB.mkString(" "))
//    println(wordsA.map(x => x.toLowerCase).filterNot(wordsB.map(x => x.toLowerCase).contains).size)
//    //task4
//    def kMostFrequentWords(strings: Array[String], k: Int): List[String] = {
//      val grouped = strings.groupBy(identity)
//      val freqs = for ((s, occurrences) <- grouped) yield (s, occurrences.size)
//      freqs.toList
//        .sortBy(-_._2)
//        .take(k)
//
//        .map(_._1)
//    }
  }
}