package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {
  behavior of "Sum of complex numbers"

  it should "be correctly" in {
    val result = ComplexNumber(3, 5) + ComplexNumber(2, 3)
    result should be (ComplexNumber(5, 8))
  }

  it should "be correctly with complex conjugate" in {
    val result = ComplexNumber(5, 3) + ComplexNumber(5, -3)
    result should be (ComplexNumber(10, 0))
  }

  it should "be correctly with negative numbers" in {
    val result = ComplexNumber(3, 5) + ComplexNumber(-5, -10)
    result should be (ComplexNumber(-2, -5))
  }

  it should "yield zero for opposited numbers" in {
    val result = ComplexNumber(3, 5) + ComplexNumber(-3, -5)
    result should be (ComplexNumber(0, 0))
  }

  behavior of "Multiply of complex numbers"

  it should "be correctly" in {
    val result = ComplexNumber(3, 5) * ComplexNumber(2, 3)
    result should be (ComplexNumber(-9, 19))
  }

  it should "be correctly with complex conjugate" in {
    val result = ComplexNumber(5, 3) * ComplexNumber(5, -3)
    result should be (ComplexNumber(34, 0))
  }

  it should "be correctly with negative numbers" in {
    val result = ComplexNumber(3, 5) * ComplexNumber(-2, -5)
    result should be (ComplexNumber(19, -25))
  }

  it should "yield zero with zero multipliers" in {
    val result = ComplexNumber(3, 5) * ComplexNumber(0, 0)
    result should be (ComplexNumber(0, 0))
  }

  behavior of "Exponentiation of complex number with integer"

  it should "be correctly" in {
    val result = ComplexNumber(3, 5) ~ 2
    result should be (ComplexNumber(-16, 30))
  }

  it should "be correctly with negative number" in {
    val result = ComplexNumber(3, 5) ~ -2
    result should be (ComplexNumber(-0.01384083045, -0.025951557093))
  }

  behavior of "Equality of two complex numbers"

  it should "be true if numbers are equal" in {
    val result = ComplexNumber(3, 5) == ComplexNumber(3, 5)
    result should be (true)
  }

  it should "be false if numbers are not equal" in {
    val result = ComplexNumber(3, 5) == ComplexNumber(3, 6)
    result should be (false)
  }

  behavior of "Printing complex number"

  it should "be correctly with floating point numbers" in {
    ComplexNumber(3.123421, 5.542123).toString should be ("3.123421 + 5.542123i")
  }

  it should "be correctly with positive real and imaginary" in {
    ComplexNumber(3, 5).toString should be ("3 + 5i")
  }

  it should "be correctly with positive real and negative imaginary" in {
    ComplexNumber(5, -2).toString should be ("5 - 2i")
  }

  it should "be correctly with negative real and positive imaginary" in {
    ComplexNumber(-5, 2).toString should be ("-5 + 2i")
  }

  it should "be correctly with negative real and imaginary" in {
    ComplexNumber(-5, -2).toString should be ("-5 - 2i")
  }

  it should "be correctly when imaginary is zero" in {
    ComplexNumber(5, 0).toString should be ("5")
  }

  it should "be correctly when real is zero" in {
    ComplexNumber(0, 5).toString should be ("5i")
  }

  it should "be correctly when real is zero and imaginary is negative" in {
    ComplexNumber(0, -5).toString should be ("-5i")
  }

  it should "be correctly when real and imaginary are zero" in {
    ComplexNumber(0, 0).toString should be ("0")
  }
}