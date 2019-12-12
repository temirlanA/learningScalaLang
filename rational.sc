object Main extends App {
  val  x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  println(x.sub(y).sub(z).toString)
  println(x.less(y).toString)
  println(x.max(y).toString)
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
  def less(that:Rational)=
    numer * that.denom < that.numer * denom
  def max (that: Rational) =
    if(this.less(that))
      that
    else
      that
  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )// constructor for method of add
  def neg: Rational = new Rational(-numer,denom)

  def sub(that: Rational) = add(that.neg)

  override def toString = {
    val g = gcd(numer,denom)
    numer/g +"/"+ denom/g
  }
}
//this class shows you how to create constructors and requirements
