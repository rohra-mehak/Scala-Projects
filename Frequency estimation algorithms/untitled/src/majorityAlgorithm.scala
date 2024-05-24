object majorityAlgorithm {
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {

    val stream = List('a', 'a' ,'a', 'c' ,'c', 'b','b' ,'c', 'c', 'c', 'b', 'c', 'c','d' ,'c', 'd', 'c')
    val result = majorityAlgorithm(stream)
    println(s"majority element  T_star , is $result")
    // program re- written with additonal n'
    verifyAlgorithm(result, stream)

  }

  def majorityAlgorithm(stream: List[Char]): Char = {
    val result = stream.foldLeft(Option.empty[Char], 0) {
      case ((candidate, count), elem) =>
        if (count == 0) {
          (Some(elem), 1)
        } else if (candidate.contains(elem)) {
          (candidate, count + 1)
        } else {
          (candidate, count - 1)
        }
    }

    result._1.getOrElse('\u0000') // Return null character if no majority element found
  }



  def verifyAlgorithm(T_star: Char, stream: List[Char]): Unit = {
    var n = 0
    var candidate = ' '
    var nPrime = 0
    stream.foldLeft(()) { (_, elem) =>
      if (n == 0) {
        candidate = elem
        n = 1
      }
      else {
        if (candidate == elem) {
          n += 1
          if (candidate == T_star) {
            nPrime += 1
          }
        }
        else {
          n -= 1
        }
      }
      if (candidate == T_star) {
        nPrime = n
      } else {
        nPrime = -n
      }

      println(s" T= $candidate ,  n = $n  , n' = $nPrime")


    }
  }
}
