import scala.math.abs

object Main extends App {
  val tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance //return bool
  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println("guess " + guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2  //f:Double => Double - this is the place to insert function

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
  sqrt(9)
}
//This code explain the highest level of abstraction in scala with carrying ability
