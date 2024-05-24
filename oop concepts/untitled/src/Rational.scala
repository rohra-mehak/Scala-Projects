import Element.elem

object Rational {

  val sep = elem('-', 3, 1)
  def fraction(numerator: Int, denom: Int): Element = {
      val num = elem(numerator)
      val denominator = elem(denom)
      denom match {
        case 0 => throw new IllegalArgumentException("Cannot divide by zero")
        case 1 => num
        case _ => num above (sep) above (denominator)
      }
    }

  def main(args: Array[String]) = {
    println(fraction(2, 3))
    println(fraction(245, 1))
    println(fraction(1, 350))
    println(fraction(1, 0))


  }

}
