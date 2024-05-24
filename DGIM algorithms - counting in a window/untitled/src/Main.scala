object Main {
  def main(args: Array[String]): Unit = {
    val (start, stop) = (1000, 1070)
    val stream = LazyList(1 until 10000: _*).map(x => if (x % 3 == 0) 1 else 0) // args(0).toInt
    val window = 100

    val dgim = new DGIM(window)
    val exact = stream.slice(start, stop + 1).sum
    println(s"Exact number of '1's in interval 1000 and 1070: $exact")

    val estimate = dgim.processStream(stream.take(stop + 1))
    println(s"Estimate number of '1's in interval 1000 and 1070: $estimate")
  }
}