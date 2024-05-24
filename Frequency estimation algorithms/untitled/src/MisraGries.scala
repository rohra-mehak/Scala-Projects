import scala.collection.mutable

class MisraGries(stream : List[String] , k: Int, m:Int) {
  private val counters: mutable.Map[String, Int] = mutable.Map.empty

  def processStream(): Unit = {
    stream.foldLeft(()) { (_, word) =>
      processWord(word)
    }
  }

  private def processWord(word: String): Unit = {
    val element = word.toLowerCase()
    if (counters.contains(element))
      counters(element) += 1
    else if (counters.size < k - 1)
      counters(element) = 1
    else
      counters.keys.foreach { key =>
        counters(key) -= 1
        if (counters(key) == 0)
          counters.remove(key)
      }
  }

def getFrequencyThreshold(m: Int): Double = m.toDouble / k

def summarizeWords() {
  val sortedCounters = counters.toList.sortBy(-_._2)
  val threshold = getFrequencyThreshold(m)

  val output = counters.keys.foldLeft(List.empty[String]) { (acc, key) =>
    val f = stream.count(_ == key)
    val lowerBound = f - threshold
    val misraGriesFrequency = counters(key)
    val isFalsePositive = misraGriesFrequency > threshold
    val estimateHolds = lowerBound <= misraGriesFrequency && misraGriesFrequency <= f
    val line = "%15s | %15s | %15s | %15s | %15s | %15s".format(key, f, lowerBound, misraGriesFrequency, isFalsePositive, estimateHolds)
    line :: acc
  }

  output.map(println)

}
}

