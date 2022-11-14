
import scala.annotation.tailrec

def factorial(n:Int): Int =
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)

@tailrec
def tail_factorial(n:Int, currentValue:Int): Int =
  if (n == 0) currentValue
  else tail_factorial(n-1, n*currentValue)

factorial(5)
tail_factorial(5, 1)
