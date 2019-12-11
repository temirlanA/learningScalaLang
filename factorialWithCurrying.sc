object factorialWithCurrying {
  def MapReduce(f: Int => Int, combine: (Int,Int)=> Int,zero:Int)(a:Int,b:Int):Int =
    if(a>b) zero
    else combine(f(a),MapReduce(f,combine,zero)(a+1,b))
  def Product(f: Int => Int)(a: Int, b: Int): Int = MapReduce(f,(x,y)=>x*y,1)(a,b)
  /*val p = Product(x => x*x)(3,4)
  print(p)*/ //144
  def fact(n:Int) = Product(x=>x)(1,n)
  val f = fact(5)
  print(f)//120
}
//Currying is a function into in another function or it is function returns function.
