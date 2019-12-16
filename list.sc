object Main extends App {
  def nth[T](n:Int,xs: List[T]): T =
    if (xs.isEmpty)
      throw new IndexOutOfBoundsException
    else if(n == 0)
      xs.head
    else
      nth(n-1,xs.tail)
  val list = new Cons(1,new Cons(2,new Cons(3, new Nil)))
  println(nth(2,list))
}

trait List[T] {
  def isEmpty : Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head:T,val tail: List[T]) extends List[T]{
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T]{
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//this code shows generic and subtypes of interface List[T]. Polymorphysm
