object Main {
  def main(args: Array[String]): Unit = {

    //task 1
    val range = 2 to 100
    val k = for (i <- range if i%2 !=0; if i%3 !=0; if i%5 !=0) yield(i * i)

    val s = range.filter(i => i % 2 != 0 && i % 3 != 0 && i % 5 != 0)
      .map(i => i * i)
    println(s==k)

    val d = for (i <- range if i%2 !=0; j <- range if j%2==0) yield(i, j)
    println(d)
    val a = range.filter(i => i % 2 != 0)
      .flatMap(i => range.filter(j => j % 2 == 0)
        .map(j => (i, j)))
    println(a)
    println(d==a)

  }
}