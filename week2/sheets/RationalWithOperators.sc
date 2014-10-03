object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x - y - z

  val a = new Rational(10, 20)
  x < y
  x max y
}

class Rational(numerator: Int, denominator: Int) {
  require(denominator != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

  private def cancel(x: Int): Int = x / gcd(numerator, denominator)

  val num = cancel(numerator)

  val den = cancel(denominator)

  def + (that: Rational) =
    new Rational(num * that.den + that.num * den, den * that.den)

  def unary_- : Rational = new Rational(-num, den)

  def - (that: Rational) = this + -that

  def < (that: Rational) = num * that.den < that.num * den

  def max(that: Rational) = if(this < that) that else this

  override def toString = num + "/" + den
}
