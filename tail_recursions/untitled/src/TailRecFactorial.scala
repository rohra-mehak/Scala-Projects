object TailRecFactorial {
  import scala.annotation.tailrec
  def tailFactorial(n: BigInt): BigInt = {
    @tailrec
    def go(acc: BigInt, n: BigInt): BigInt = {
      if (n <= 1) acc
      else go(n * acc, n - 1)
    }
    go(1, n)
  }

//  @tailrec
  //recursive call not in tail position
  // the recursive call is not the last thing the function does
  def factorial(n: BigInt): BigInt = {
    if (n <= 1) 1
    else n * factorial(n-1)
  }


  def tailFib(n: BigInt): BigInt = {
    @tailrec
    def go( n :BigInt,acc: BigInt, acc2 : BigInt): BigInt = {
      if (n == 0) acc
      else go(n- 1, acc2, acc+acc2)
    }
    go(n, 0, 1)
  }


  def main(args:Array[String]):Unit = {
    var x = "f"
//    var x = "2"
    try {
     val n = x.toInt
//      println(tailFib(n))
//      println(tailFactorial(n))
      println(factorial(n))
    }
    catch {
      // Catch block contain cases.
      case i: NumberFormatException => {
        println("Number format exception  occurred.")
      }
    }
//    println(map2(Some(10), Some(20))(((a:Int, b:Int) => a -b)))

  }


  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (x => b map (y => f(x, y)))
  }

  val sub = (a:Int, b:Int) => a -b
  map2(Some(10), Some(20))(sub)





  //  task 1.4
  // minimal n leading to stackoverflow in factorial(n) is around ~5799 to 6000
}

//task 3
//val s = fileSourceA.mkString("")
//val substrings = (0 to s.length - 24).map(i => s.substring(i, i + 24)).toList
//substrings.find(substring => MurmurHash3.stringHash(substring) == 320915200)

//task 4
//val substrings = (0 to s.length - 24).map(i => s.substring(i, i + 24)).toList
//
//for (x <- substrings) {
//  if (MurmurHash3.stringHash(x) == 320915200) {
//    x
//  }
//  else println("not found")
//}


//

