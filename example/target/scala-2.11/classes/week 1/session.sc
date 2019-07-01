

def sqrt(x: Double): Double = {
  def abs(x: Double) : Double =
    if (x < 0) -x else x

  def goodEnough(guess: Double) : Boolean =
    abs(guess * guess - x) / x < 0.001

  def improveGuess(guess: Double) : Double =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double) : Double =
    if (goodEnough(guess)) guess
    else sqrtIter(improveGuess(guess))

  sqrtIter(1.0)
}



2+2
sqrt(2)