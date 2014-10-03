object rationals {
  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.sub(y).sub(z)

  val a = new Rational(10, 20)
  x.less(y)
  x.max(y)
}

class Rational(numerator: Int, denominator: Int) {
  require(denominator != 0, "denominator must be nonzero")
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

  private def cancel(x: Int): Int = x / gcd(numerator, denominator)

  val num = cancel(numerator)

  val den = cancel(denominator)

  def add(that: Rational) =
    new Rational(num * that.den + that.num * den, den * that.den)

  def neg = new Rational(-num, den)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = num * that.den < that.num * den

  def max(that: Rational) = if(this.less(that)) that else this

  override def toString = num + "/" + den
}
