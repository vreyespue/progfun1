package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    // if c == 0 || c == r then 1
    // else pascal(c - 1, r - 1) + pascal(c, r - 1)
    (c, r) match
      case (0, _)           => 1
      case (c, r) if c == r => 1
      case (c, r)           => pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean =

    @tailrec
    def doCountOpenPars(chars: List[Char], currentCount: Int): Boolean =
      if chars.isEmpty then true
      else
        chars.head match
          case '(' => doCountOpenPars(chars.tail, currentCount + 1)
          case ')' =>
            currentCount > 0 && doCountOpenPars(chars.tail, currentCount - 1)
          case _ => doCountOpenPars(chars.tail, currentCount)

    if chars.isEmpty then true
    else doCountOpenPars(chars, 0)

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    def countChangeOrd(money: Int, coins: List[Int]): Int =
      if coins.isEmpty || money < 1 then 0
      else if coins.tail.isEmpty then
        if money > 0 && money % coins.head == 0 then 1
        else 0
      else if coins.head == money then 1 + countChangeOrd(money, coins.tail)
      else if coins.head > money then countChangeOrd(money, coins.tail)
      else
        1 * countChangeOrd(money - coins.head, coins) +
          countChangeOrd(money, coins.tail)

    countChangeOrd(money, coins.reverse)
