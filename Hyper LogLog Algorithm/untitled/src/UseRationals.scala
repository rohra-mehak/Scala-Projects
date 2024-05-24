object UseRationals {

  import Rational._

  import scala.language.implicitConversions

  implicit def intToRational(x: Int) = new Rational(x)

  import Element.elem



  def main(args:Array[String]): Unit = {



    val lhs = elem(frac(3, 5)) + elem(frac(2, 14))

    val rhs = elem(frac(3, 5) + frac(2, 14))

    println(lhs equals rhs)



    // produces

    //3   1    26

    //- + - = --

    //5   7    35



    val lhs_2 = elem(frac(3, 9)) + elem(frac(2, 14))

    val rhs_2 = elem(frac(3, 9) + frac(2, 14))

    println(lhs_2 equals rhs_2)



    // produces

    //1   1   10

    //- + - = --

    //3   7   21





    val lhs_3 = elem(frac(-59, 689)) + elem(frac(-2, 14))

    val rhs_3 = elem(frac(-59, 689) + frac(-2, 14))

    println(lhs_3 equals rhs_3)

    // produces



    //-59   -1   -1102

    //--- + -- = -----

    //689   7    4823



  }



  def frac(n: Int, d: Int): Rational = Rational(n, d)



}