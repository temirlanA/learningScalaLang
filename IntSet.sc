object Main extends App {
  val t1 = new NoneEmpty(3, Empty,  Empty)
  val t2 = t1 incl 4
  val t3 = t1 incl 2

  println(t1)//{.3.}
  println(t2)//{.3{.4.}}
  println(t3)//{{.2.}3.}

  println(new NoneEmpty(7,Empty,Empty) contains 7)//true
}

abstract class IntSet{//superclass
  def incl(x:Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet{//subclass and singleton object
  def incl(x: Int): IntSet = new NoneEmpty(x,Empty, Empty )//singleton object don't need to new instance

  def contains(x: Int): Boolean = false

  def union(other: IntSet): IntSet = other

  override def toString = "."//to override toString because if is global function
}

class NoneEmpty(elem: Int,left: IntSet,right: IntSet) extends IntSet{//subclass
  def incl(x: Int): IntSet =
    if(x < elem) new NoneEmpty(elem,left incl x,right)
    else if(x > elem) new NoneEmpty(elem,left,right incl x)
    else this

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}

//1.0 this code shows how to use abstraction in scala with inheritance
//and simple binare tree
