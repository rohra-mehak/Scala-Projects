
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object Main {
  def main(args: Array[String]): Unit = {

    val n = Math.pow(2, 23).toInt
    val xs : LazyList[Int] = {
      val random = new Random()
      LazyList.continually(random.nextInt()).take(n)
    }
    stochasticCounting(xs)

  }


  def stochasticCounting1(xs: LazyList[Int], m: Int = 0): Int = xs match {
    case LazyList() => m
    case x #:: tail =>
      val rho = Integer.numberOfLeadingZeros(x) + 1
      stochasticCounting1(tail, if (rho > m) rho else m)
  }

//  def stochasticCounting(xs: LazyList[Int], Z: Array[Int] = Array.fill(256)(0), n: Int = 0): Array[Int] = xs match {
//    case LazyList() => Z
//    case pi #:: tail =>
//      val bucket = math.floorMod(pi, 256)
//      val rho = Integer.numberOfLeadingZeros(pi) + 1
//            if (rho > Z(bucket)) {
//              Z(bucket) = rho
//            }
//      val newZ = Z.clone()
//      stochasticCounting(tail, newZ, n + 1)
//  }

  def stochasticCounting2(xs: LazyList[Int]): Array[Int]= {
    val Z = Array.fill(256)(0)
    val newZ = xs.foldLeft(Z) { (acc, num) =>
      val bucket = num & 0xFF
      acc.updated(bucket, acc(bucket) + 1)
    }

    val Zj = newZ.map(rho => if (rho > 0) 1 << (rho - 1) else 0)
    Zj
  }

  def stochasticCounting(xs: LazyList[Int]): Unit = {

    val m = 256
    val z = Array.fill(m)(0)
    val (mMax, zUpdated) = xs.foldLeft((1, z)) {
      case ((m, z), x) =>
        val bucketIndex = Math.abs(x % m)
        val rho = Integer.numberOfLeadingZeros(x) + 1
        val mMaxUpdated = Math.max(m, rho)
        z(bucketIndex) += 1
        (mMaxUpdated, z)

    }
    //results

    val zMean = zUpdated.sum.toDouble / m

    val Zharmonicavg = m.toDouble / zUpdated.map(1.0 / _).sum

    val elems = xs.distinct.length

    val mZavg = m * zMean

    val mZh = m * Zharmonicavg


  }


}