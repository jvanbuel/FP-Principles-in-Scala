package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c % r == 0) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_count(chars: List[Char], n_open: Int): Boolean = {
      if (chars.isEmpty) (n_open == 0)
      else if (chars.head == ')' && n_open == 0) false
      else if (chars.head == ')' && n_open != 0) balance_count(chars.tail, n_open - 1)
      else if (chars.head == '(') balance_count(chars.tail, n_open + 1)
      else balance_count(chars.tail, n_open)
    }

    balance_count(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (!coins.isEmpty)
      if (money < 0 ) 0
      else if (money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }

}
