object MultPoly {
  def mult(a: Int, b: Int): Int = {
    @scala.annotation.tailrec
    def multHelper(a: Int, b: Int, acc: Int): Int = {
      if (a == 0 || b == 0) acc
      else if ((a & 1) == 1) multHelper(a >>> 1, b << 1, acc ^ b)
      else multHelper(a >>> 1, b << 1, acc)


    }

    multHelper(a, b, 0)
  }

  def main(args: Array[String]): Unit = {
    val a = 11
    val b = 23
    println(mult(a, b))
  }
}
