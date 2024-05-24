import scala.math._
object Main {
  def main(args: Array[String]): Unit = {
//    val my_list = List(1,2, 3,4,5 ,6)
//    println(lengthRight[Int](my_list))


//    println((fM _ andThen gM)("y"))
//    println((fM _ andThen gM _)("y"))



//    (fM _ andThen gM(_: String, '[', ']'))("x")
    def f(a:Int, b:Int ) : Int = {
        a + b*2
    }

//    println(curry(f)(1)(2) == f(1 ,2))
//
//    println(uncurry(curry(f))(3, 4) == f(3, 4))
//


    val my_exp = (a: Double) => exp(a)
    val my_log = (a: Double) => log(a)
    println(cmp(my_exp , my_log)(4.0))


//
//    import math._
//    val x = List.range(0, 10).map(_.toDouble)
//    val xx = x.map(exp _ andThen log _)
//    val xxx = x.map(exp _ compose log _)

  }

   //task1
  def lengthRight[A](as: List[A]): Int = {
      as.foldRight(0)((_, elem) => elem +1 )
  }

  //task2
  def fM(x: String): String = "f(" + x + ")"
  def gM(x: String): String = "g(" + x + ")"

  var fF: String => String = (x: String) => "f(" + x + ")"
  var gF: String => String = (x: String) => "g(" + x + ")"

  //1.
  (fM _ andThen gM _)("x")
  (fF andThen gF)("x")

  //2.
  def gM(x: String, leftPar: Char, rightPar: Char): String = {
    "g" + leftPar + x + rightPar
  }

  // first one doesn't work as we are passing the return value of gM which is a string
//  (fM _ andThen gM(_: String, '[', ']'))("x")
  // second one works as we are passing an instance of the function itself, and not String .
  (fM _ andThen (gM(_: String, '[', ']')))("x")




  //task3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def f(a: Int, b: Int): Int = {
    a + b * 2
  }


  //task4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a, b) => f(a)(b)
}

 //task5
  //1.
 def cmp[A,B,C](f: B => C, g: A => B): A => C = {
   x => f(g(x))
 }

  //2.


  //3.

  import math._

  val x = List.range(0, 10).map(_.toDouble)
  val xx = x.map(exp _ andThen log _)
  val xxx = x.map(exp _ compose log _)

//  xx and xxx are not equal as
//  "and then" first applies the input to its first function and passes the result
//  of
//  this to the next function.
//    compose on the other hand first calculates the result by passing the input to the inner function that is log (x) in this
//  case and then passes the result of this to evaluate the outer function that is the exponent in
//  this
//  case.
}
