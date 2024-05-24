object Main {
  def main(args: Array[String]): Unit = {

     println(indexFib(1000))

    val a = List.range(1 , 1000, 1)
    println(sumAmicableNumbers(a))


  }

  val fibs: LazyList[BigInt] = LazyList.unfold((BigInt(0), BigInt(1))) {
    case (x, y) => Some((x, (y, y + x)))

  }
  def indexFib(d: Int): Int = {
    fibs.takeWhile(_.toString.length < d).length
  }




  //task 2 amicalble pairs
  def sumOfDivisors(a: Int): Int = {
    val divisors = (1 to a / 2).filter(a % _ == 0)
    divisors.sum
  }

  def isAmicable(a: Int, b: Int): Boolean = {
    sumOfDivisors(a) == b && sumOfDivisors(b) == a && a!=b
  }

  def sumAmicableNumbers(list: List[Int]): Int = {
      val amicablePairs = list.combinations(2).filter {
        case List(a, b) => isAmicable(a, b)
      }.toList

      val amicableNumbers = amicablePairs.flatten.distinct
      amicableNumbers.filter(a => amicablePairs.exists(_.contains(a))).sum

  }

}




