object A3  {

  def main(args: Array[String]): Unit = {
    val (start, stop) = (1000, 1070)
    val stream = LazyList(1 until 100000: _*).map(x => if (x%3 == 0) 1 else 0) // args(0).toInt
    val exact = stream.slice(start, stop + 1).sum
    println(s"Exact number of '1's: $exact")

    val window = 100
    val k = stop - start + 1
    var arr_buckets = Array[(Int, Int)]()
    val stream2 = stream.take(stop + 1).zipWithIndex
    var idx = 0
    var elem = Array[(Int, Int)]()
    stream2.foreach(i => if (i._1 == 1) {
      arr_buckets = i+: arr_buckets.filter(_._2 >= i._2 - window) // adding new bucket (size=1), so just one 1,
      // but first remove buckets with timestamp bigger then current timestamp - window
      arr_buckets.map(x => if (arr_buckets.count(_._1 == x._1) > 2) {
        elem = arr_buckets.filter(_._1 == x._1)
        idx = arr_buckets.indexOf(elem(0)) + 1 // getting index of the second bucket with the same size
        arr_buckets(idx) = (arr_buckets(idx)._1 * 2, arr_buckets(idx)._2)
        arr_buckets = arr_buckets.patch(idx + 1, Nil, 1)  // removing 3d bucket of same size
        x} else x) // anyway we leave current element in a bucket
    })
    // println(stream2.slice(start, stop + 1))
    println(s"Buckets list: ${arr_buckets.toList}")
    val estimate = arr_buckets.last._1 / 2 + arr_buckets.dropRight(1).map(_._1).sum
    println(s"Estimate number of '1's: ${estimate}")

  }
}