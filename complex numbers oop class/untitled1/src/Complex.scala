package com.asg4.app

import scala.language.implicitConversions

class Complex(val real : Double , val imaginary : Double) {

  // AUX CONSTRUCTOR
  def this(real: Double) = this(real, 0.0)

  override def toString: String = {
    if (imaginary < 0) {
      s"$real - ${math.abs(imaginary)}i"
    }
    else {
      s"$real + ${math.abs(imaginary)}i"
    }

  }

  // Addition operation
  def +(that: Complex): Complex = {
    new Complex(this.real + that.real, this.imaginary + that.imaginary)
  }

  // Subtraction operation
  def -(that: Complex): Complex = {
    new Complex(this.real - that.real, this.imaginary - that.imaginary)
  }

  // multiply to a scalar
  def *(other: Double): Complex = {
    new Complex(real * other, imaginary * other)
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


  def abs: Double = math.sqrt(real * real + imaginary * imaginary)
  def conj: Complex = new Complex(real, -imaginary)

}


object Complex {
  // Creating a complex number from a real and an imaginary part
  def apply(real: Double, imaginary: Double): Complex = {
    new Complex(real, imaginary)
  }
  // Creating a complex number from a real part only.
  def apply(real: Double) = new Complex(real)

  // FOR IMPLICIT CONVERSIONS
  implicit def doubleToComplex(d: Double): Complex = new Complex(d, 0.0)

}
