package recfun

import scala.collection.mutable.ListBuffer


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
  def pascal(c: Int, r: Int): Int =
    if(c==0 || c==r) 1
    else pascal(c-1,r-1) + pascal(c,r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def f(chars: List[Char], countOpens: Int): Boolean = {

      if(chars.isEmpty) {countOpens == 0}

      else
      {
        val char = chars.head
          val next =
            if (char == '(') countOpens + 1
            else if (char == ')') countOpens - 1
            else countOpens
          if (next >= 0) f(chars.tail, next)
          else false
      }
    }

    f(chars, 0)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def function(lastMaxTotal: List[(Int, Int)], count: Int): Int = {

      if (lastMaxTotal.isEmpty) { count }

      else
      {
        val b = ListBuffer[(Int, Int)]()
        var newCount = count
        for ((lastMaxCoin, total) <- lastMaxTotal) {
          if (total < money) {
            for (c <- coins) {
              if (c >= lastMaxCoin) {
                val e = (c, total + c)
                b += e
              }
            }
          } else if (total == money) {
            newCount += 1
          }
        }

        function(b.toList, newCount)
      }
    }

    val b = coins.map { c => (c, c) }

    function(b, 0)
  }
}
