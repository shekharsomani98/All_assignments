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
  if(c==0||c == r) 1
  else if (c>r||c<0) 0
  else pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def pcount(cnt:Int, chr:Char,chars:List[Char]): Unit ={
      if (chr == '(') cnt + 1
      else if(chr == ')') cnt - 1

      if(cnt<0) cnt
      else if(chars.isEmpty) cnt
      else pcount(cnt,chars.head,chars.tail)
    }
    pcount(0,chars.head,chars.tail) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money<0||coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
