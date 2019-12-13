object Main extends App {
  val  x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  println(x - y - z toString)//x.sub(y).sub(z) old
  println(x < y toString)//x.less(y)
  println(x.max(y).toString)//didn't change 
  println(new Rational(2).toString)
}



class Rational(x: Int, y: Int){
  require(y>0, "denominator must be positive!")//requirement for y

  def this(x: Int) = this (x,1)//constructor of class
  private  def gcd(a:Int,b:Int):Int =
    if(b==0)
      a
    else
      gcd(b,a%b)

  def numer = x//get y and set to numer
  def denom = y
  def <(that:Rational)=
    numer * that.denom < that.numer * denom// i changed less to <
  def max (that: Rational) =
    if(this < that)
      that
    else
      that
  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )// constructor for method of add
  def unary_- : Rational = new Rational(-numer,denom)//i changed neg to unary_-

  def -(that: Rational) = this + that unary_- //i changed sub to -

  override def toString = {
    val g = gcd(numer,denom)
    numer/g +"/"+ denom/g
  }
}
//1.0 this class shows you how to create constructors and requirements
//1.1 i changed strings to signs because Int != Rational and types of Rational can't to use like integer numbers like a+b
//and i fixed that things
