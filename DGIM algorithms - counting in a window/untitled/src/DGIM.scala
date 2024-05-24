class DGIM(windowSize: Int) {
  private var buckets = List[(Int, Int)]()

  def processStream(stream: LazyList[Int]): Int = {
    var estimate = 0
    var idx = 0
    var elem = List[(Int, Int)]()

    stream.zipWithIndex.foreach { case (value, timestamp) =>
      if (value == 1) {
        buckets = (1, timestamp) :: buckets.filter(_._2 >= timestamp - windowSize)

        buckets = buckets.map { case (size, ts) =>
          if (buckets.count(_._1 == size) > 2) {
            elem = buckets.filter(_._1 == size)
            idx = buckets.indexOf(elem(0)) + 1
            buckets = buckets.updated(idx, (buckets(idx)._1 * 2, buckets(idx)._2))
            buckets = buckets.patch(idx + 1, Nil, 1)
            (size, ts)
          } else {
            (size, ts)
          }
        }
      }

      if (buckets.nonEmpty) {
        estimate = buckets.last._1 / 2 + buckets.dropRight(1).map(_._1).sum
      }
    }

    estimate
  }
}

