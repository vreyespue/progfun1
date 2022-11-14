
def abs(x:Double) = if (x < 0) -x else x

def isGoodEnough(guess: Double, x: Double) =
  if (abs(guess * guess - x) < x/1000) true else false

def improve(guess: Double, x: Double) =(guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def mysqrt(x: Double) = sqrtIter(1.0, x)

mysqrt(2)
mysqrt(0.001)
mysqrt(0.1e-20)
mysqrt(1.0e20)
mysqrt(1.0e50)
