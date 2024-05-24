// Developing HyperLogLog alg



import scala.util.hashing.MurmurHash3



class HyperLogLog(val b: Int) {

  val m: Int = math.pow(2, b).toInt



  private val M = new Array[Int](m)

  private val alphaMM = getAlphaMM(m)

  private val smallRangeThreshold = 2.5 * m

  private val largeRangeThreshold = 1.0 / 30.0 * Math.pow(2, 32)



  private def getAlphaMM(m: Int): Double = {

    m match {

      case 16 => 0.673

      case 32 => 0.697

      case 64 => 0.709

      case _ => 0.7213 / (1 + 1.079 / m.toDouble)

    }

  }



  private def hash(x: String): Int = {

    MurmurHash3.stringHash(x)

  }



  private def getBucket(h: Int): Int = {

    val mask = (1 << b) - 1

    h & mask

  }



  private def getLeadingZeros(h: Int): Int = {

    Integer.numberOfLeadingZeros( h ) + 1

  }



  def add(x: String): Unit = {

    val h = hash(x)

    val bucket = getBucket( h )

    val lz = getLeadingZeros(h >>> b)

    M(bucket) = Math.max(M(bucket), lz)

  }



  def count(): Double = {

    val mF = m.toDouble

    val MSum = M.map(x => Math.pow(2, -x)).sum

    val z = 1.0 / MSum



    if (z <= smallRangeThreshold) {

      val V = M.count(_ == 0)

      if (V > 0) {

        mF * Math.log(mF / V)

      } else {

        z

      }

    } else if (z <= largeRangeThreshold) {

      z * alphaMM * mF * mF

    } else {

      -Math.pow(2, 32) * Math.log(1 - z / Math.pow(2, 32))

    }

  }

}