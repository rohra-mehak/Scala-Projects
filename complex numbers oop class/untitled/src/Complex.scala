class Complex(real : Double, imaginary : Double) {
  override def toString: String = {
    s"$real + ${imaginary}i"
  }

  // Addition operation
  def +(that: Complex): Complex = {
    new Complex(this.real + that.real, this.imaginary + that.imaginary)
  }

  // Subtraction operation
  def -(that: Complex): Complex = {
    new Complex(this.real - that.real, this.imaginary - that.imaginary)
  }

  // Multiplication operation
  def *(that: Complex): Complex = {
    new Complex(this.real * that.real - this.imaginary * that.imaginary,
      this.real * that.imaginary + this.imaginary * that.real)
  }

  // Division operation
  def /(that: Complex): Complex = {
    val denominator = that.real * that.real + that.imaginary * that.imaginary
    new Complex((this.real * that.real + this.imaginary * that.imaginary) / denominator,
      (this.imaginary * that.real - this.real * that.imaginary) / denominator)
  }
}

object Complex {
  // Creating a complex number from a real and an imaginary part
  def apply(real: Double, imaginary: Double): Complex = {
    new Complex(real, imaginary)
  }

}
