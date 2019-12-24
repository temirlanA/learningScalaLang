package Main

object Main extends App {

  val nums = List(1, 2, 3, 4)
  val nums2 = 1 :: 2 :: 3 :: 4 :: Nil
  println(nums.equals(nums2))//true

  val fruit = List("apples", "oranges", "pears")
  val fruit2 = "apples" :: ("oranges" :: ("pears" :: Nil))
  println(fruit.equals(fruit2))//true

  val diag3 =
    List(
      List(1, 0, 0),
      List(0, 1, 0),
      List(0, 0, 1)
    )
  val empty = List()




  val toSortList = isort(List(-4,5,1,-7,0,3))
  println(toSortList)//List(-7, -4, 0, 1, 3, 5)

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case x :: xs1 => insert(x, isort(xs1))
  }
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs
    else y :: insert(x, ys)

  }

  val concatList = List(1, 2) ::: List(3, 4, 5)//List[Int] = List(1, 2, 3, 4, 5)


  val abcde = List('a', 'b', 'c', 'd', 'e')
  val edcba = abcde.reverse
  println(abcde + " and " + edcba)//List(a, b, c, d, e) and List(e, d, c, b, a)

  val ab= abcde take 2
  println(ab)//List(a, b)

  val abSplitcde = abcde splitAt(2)
  println(abSplitcde)//(List(a, b),List(c, d, e))

  val dropAB = abcde.drop(2)
  println(dropAB)//List(c, d, e)

  val flattenFruit = fruit.map(_.toCharArray).flatten//
  println(flattenFruit)//List(a, p, p, l, e, s, o, r, a, n, g, e, s, p, e, a, r, s)


  def msort[T](less: (T, T) => Boolean)
              (xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs))
    }
  }


  val mergeSort = msort((x: Int, y: Int) => x < y)(List(5, 7, 1, 3))
  println(mergeSort)//List(1, 3, 5, 7)

  val intSort = msort((x: Int, y: Int) => x < y) _
  val mixedInts = List(4, 1, 9, 0, 5, 8, 3, 6, 2, 7)
  val intSortPass = intSort(mixedInts)
  println(intSortPass)//List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)


  var sum = 0
  List.range(1,6) foreach( sum += _ )//1 + 2 + 3 + 4 + 5
  println(sum)//15

  val dividedTo2 = List(1, 2, 3, 4, 5) filter (_ % 2 == 0)
  println(dividedTo2)//List(2, 4)

  val separationToEvenAndOdd = List(1, 2, 3, 4, 5) partition(_ % 2 == 0)
  println(separationToEvenAndOdd)//(List(2, 4),List(1, 3, 5))

  val findAndConvertToMonad = List(1, 2, 3, 4, 5) find (_ % 2 == 0)
  println(findAndConvertToMonad)//Some(2)

  val takeOnlyNaturalNumbers = List(1, 2, 3, -4, 5) takeWhile (_ > 0)
  println(takeOnlyNaturalNumbers)//List(1, 2, 3)

  val words = List("the", "quick", "brown", "fox")
  val sentence = (words.head /: words.tail) (_ +" "+ _)
  println(sentence)//the quick brown fox

  val highOrderSort = List(1, -3, 4, 2, 6) sortWith (_ < _)
  println(highOrderSort)//List(-3, 1, 2, 4, 6)

  val concatList2 = List.concat(List('a', 'b'), List('c'))
  println(concatList2)//List(a, b, c)
}
