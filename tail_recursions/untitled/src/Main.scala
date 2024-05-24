object Main {
  import scala.util.hashing.MurmurHash3
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }

  def intToString(x: Int): String = {
    val s = x.toString
    if (s.length < 4) {
      ("A" * (4 - s.length)) + s.map(_.toChar).mkString
    } else {
      s.map(_.toChar).mkString
    }
  }

  5.2

  def hashSequence(s: String, iterations: Int): List[Int] = {
    if (iterations == 0) {
      Nil
    } else {
      val hash = MurmurHash3.stringHash(s)
      hash :: hashSequence(intToString(hash), iterations - 1)
    }
  }

  5.3

  def findPassword(targetHash: Int, iterations: Int): Option[String] = {
    val initialString = "AAAA"
    val hashes = hashSequence(initialString, iterations)
    hashes.zipWithIndex.find { case (hash, index) =>
      if (hash == targetHash) {
        Some(intToString(MurmurHash3.stringHash(initialString, MurmurHash3.arraySeed(index))))
      } else {
        None
      }
    }.flatten
  }

  val initialString = "AAAA"
  val iterations = 100
  val hashes = hashSequence(initialString, iterations)

  // Find the password corresponding to the 100th hash
  val targetHash = hashes(99)
  val password = findPassword(targetHash, iterations)
  println(password)

}