package com.asg4.app
import com.asg4.app.Complex

object DemoComplex {
  def main(args: Array[String]): Unit = {
    val c1 = new Complex(2.0, -3.0)
    val c2 = new Complex(1.0, 5.0)

    val c3 = c1 + c2 // adds two complex numbers
    val c4 = c1 - c2 // subtracts two complex numbers
    val c5 = c1 * c2 // multiplies two complex numbers
    val c6 = c1 / c2 // divides two complex numbers

    println(c1)
    // produces 2.0 - 3.0i
    println(c2)
    // produces  1.0 + 5.0i
    println(c3, c4 , c5, c6)
    // produces (3.0 + 2.0i, 1.0 - 8.0i , 17.0 + 7.0i ,-0.5 - 0.5i)

    val zA = new Complex(-6.0, 1.0)
    println(zA)
    //produces   -6.0 + 1.0 i
    val zB = Complex(5.0, 1.0)
    println(zB)
    //produces 5.0 + 1.0 i
    val zC = Complex(3.0)
    println(zC)
    // produces 3.0 + 0.0i
    val a = zA * zA.conj
    //produces 37.0 + 0.0i
    println(a)
    val b = (5.0 * zA + 7.0 * zB).abs
    println(b)
    // produces 13.0

  }
}
