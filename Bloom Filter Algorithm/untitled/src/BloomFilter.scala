import scala.util.hashing.MurmurHash3

class BloomFilter(m: Int, k: Int) {
  private val bitArray = Array.fill(m)(false)
  private val hashFunctions = Array.tabulate(k) { i =>
    val seed = i + 1 // Use different seed values for each hash function
    (input: Array[Byte]) => Math.abs(MurmurHash3.bytesHash(input, seed) % m)
  }

  def add(element: String): Unit = {
    hashFunctions.foreach { hashFunction =>
      val index = hashFunction(element.getBytes)
      bitArray(index) = true
    }
  }

  def contains(element: String): Boolean = {
    hashFunctions.forall { hashFunction =>
      val index = hashFunction(element.getBytes)
      bitArray(index)
    }
  }

  def falsePositiveRate(actualSize: Int): Double = {
    val expectedSize = math.pow(1 - math.exp(-k * actualSize.toDouble / m), k)
    math.pow(1 - math.exp(-k * actualSize.toDouble / m), k)
  }

  def distinctCount: Double = {
    val ones = bitArray.count(identity)
    -(m.toDouble / k) * Math.log(1 - ones.toDouble / m)
  }
}
