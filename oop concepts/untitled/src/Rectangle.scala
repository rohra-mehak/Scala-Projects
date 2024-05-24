import Element.elem
object Rectangle {
  val corner = elem("*")

  def rect(width: Int, height : Int) : Element = {
    require(width >=2 & height >= 2)
    (width , height) match {
      case ( _ , 2 ) =>
        val topAndBottom = corner beside elem('=', width - 2, 1) beside corner
        topAndBottom above (topAndBottom)
      case (2,2) =>
        val topAndBottom = corner beside corner
        topAndBottom above (topAndBottom)
      case ( 2,_) =>
        val topAndBottom = corner beside corner
        val vLine = elem('|', 1, height - 2)
        topAndBottom above (vLine beside vLine) above (topAndBottom)
      case (_, _ ) =>
        val hLine = elem('=', width - 2, 1)
        val vLine = elem('|', 1, height - 2)
        val middle = vLine beside elem(' ', width - 2, height - 2) beside vLine
        val topAndBottom = corner beside hLine beside corner
        topAndBottom above middle above topAndBottom
    }

  }

  def main(args: Array[String]) = {
    println(rect(width = 3, height = 2))
    println(rect(width = 2, height = 3))
    println(rect(2,2))
    println(rect(5, 4))
    println(rect(5,5))

  }

}

