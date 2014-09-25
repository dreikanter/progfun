package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  // def pascal(c: Int, r: Int): Int = {
  //   if (c < 0 || c > r) 0
  //   else if (c == 0 || c == r) 1
  //   else if (c == 1 || c == r - 1) r
  //   else if (c == 2 || c == r - 2) (r - 1) * r / 2
  //   else pascal(c - 1, r - 1) + pascal(c, r - 1)
  // }

  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) 0
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r) * (r - c + 1) / c
  }


  // Next variation fails one of the Coursera tests:
  //
  //   [Test Description] pascal: symmetry
  //   [Observed Error] org.scalatest.exceptions.TestFailedException:
  //   22 did not equal 21
  //     [exception was thrown] detailed error message in debug output
  //     section below
  //   [Lost Points] 10

  // def pascal(c: Int, r: Int): Int = {
  //   def pas(c: Double, r: Double, k: Double): Double = {
  //     if (c < 0 || c > r) 0
  //     else if (c == 0 || c == r) k
  //     else pas(c - 1, r, (r - c + 1) / c * k)
  //   }
  //   pas(c, r, 1).toInt
  // }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def bal(chars: List[Char], depth: Int): Boolean = {
      if (depth < 0) false
      else if (chars.isEmpty) depth == 0
      else if (chars.head == '(') bal(chars.tail, depth + 1)
      else if (chars.head == ')') bal(chars.tail, depth - 1)
      else bal(chars.tail, depth)
    }
    bal(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money > 0) 0
    else countChange(money, coins.tail) +
         countChange(money - coins.head, coins)
  }
}
