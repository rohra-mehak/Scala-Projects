// adding rational element class, elem defintion and required equals and + methods 

object Element {



  private class ArrayElement(

                              val contents: Array[String]

                            ) extends Element



  private class LineElement(s: String) extends Element {

    val contents = Array(s)

    override def width = s.length

    override def height = 1

  }



  private class UniformElement(

                                ch: Char,

                                override val width: Int,

                                override val height: Int

                              ) extends Element {

    private val line = ch.toString * width

    def contents = Array.fill(height)(line)

  }



  private class NumberElement(n: Int) extends Element {

    val contents = Array(n.toString)



    override def width = n.toString.length



    override def height = 1

  }





  // add a RationalElement that extends Element in Element scala file

  // this uses Rational class given with the assignment as well.



  class RationalElement(r: Rational) extends Element {

    override def contents: Array[String] = {

      val numerElem = elem(r.numer)

      val denomElem = elem(r.denom)

      val width = math.max(numerElem.width, denomElem.width)

      val barElem = elem('-', width, 1)

      val fractionElem = numerElem above barElem above denomElem

      fractionElem.contents

    }



  }



  def elem(contents:  Array[String]): Element =

    new ArrayElement(contents)



  def elem(chr: Char, width: Int, height: Int): Element =

    new UniformElement(chr, width, height)



  def elem(line: String): Element = new LineElement(line)



  def elem(n: Int): Element = new NumberElement( n )





  // adding element definition :



  def elem(r: Rational): RationalElement = new RationalElement(r)



}



import Element.elem



abstract class Element {

  def contents:  Array[String]

  def width: Int = contents(0).length

  def height: Int = contents.length



  def above(that: Element): Element = {

    val this1 = this widen that.width

    val that1 = that widen this.width

    elem(this1.contents ++ that1.contents)

  }



  def beside(that: Element): Element = {

    val this1 = this heighten that.height

    val that1 = that heighten this.height

    elem(

      for ((line1, line2) <- this1.contents zip that1.contents)

        yield line1 + line2)

  }



  // adding an equals and + methods in abstract class



  def equals(that: Element): Element = {

    val eq = elem('=', 1, 1)

    this beside elem(' ', 1, 1) beside (eq) beside (elem(' ', 1, 1)) beside (that)

  }



  def +(that: Element): Element = {

    this beside  elem(' ', 1, 1) beside elem('+', 1, 1) beside  elem(' ', 1, 1) beside ( that)

  }



  def widen(w: Int): Element =

    if (w <= width) this

    else {

      val left = elem(' ', (w - width) / 2, height)

      val right = elem(' ', w - width - left.width, height)

      left beside this beside right

    }



  def heighten(h: Int): Element =

    if (h <= height) this

    else {

      val top = elem(' ', width, (h - height) / 2)

      val bot = elem(' ', width, h - height - top.height)

      top above this above bot

    }



  override def toString = contents mkString "\n"

}