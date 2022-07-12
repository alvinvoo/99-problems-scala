//https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html
// problem 1
// how generics method work
def myLast[A](list: List[A]) = list.last

// problem 2
def myButLast[A](list: List[A]) = list.slice(list.length - 2, list.length)

// problem 3
def elementAt[A](list: List[A], index: Int) = list(index)

// problem 4
def numberOfElements[A](list: List[A]) = list.length

// problem 5
def reverseList[A](list: List[A]) = list.reverse

// problem 6
def palindrome[A](list: List[A]) = list == list.reverse

// problem 7
// usage
// scala> myFlatten(List(List(1,List(2.1,2.2)),3,List(4,5)))
// val res2: List[Matchable] = List(1, 2.1, 2.2, 3, 4, 5)
def myFlatten[Any](list: List[Any]): List[Any] =
  if list.isEmpty then
    List()
  else
    list.head match 
      case _: List[Any] => myFlatten(list.head.asInstanceOf[List[Any]]) ++ myFlatten(list.tail)
      case _ => List(list.head) ++ myFlatten(list.tail)

// problem 8
//scala> compress(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e'))
//val res5: List[Char] = List(a, b, c, a, d, e)
def compress[A](list: List[A]): List[A] =
  if list.isEmpty then
    List()
  else
    List(list.head) ++ compress(list.tail.dropWhile(_ == list.head))

// problem 9
// scala> pack(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e'))
// val res18: List[List[Char]] = List(List(a, a, a, a), List(b), List(c, c), List(a, a), List(d), List(e, e, e, e))
def pack[A](list: List[A]): List[List[A]] =
  if list.isEmpty then
    List()
  else
    List(List(list.head) ++ list.tail.takeWhile(_ == list.head)) ++ 
    pack(list.tail.dropWhile(_ == list.head))

// problem 10
// scala> encode(List('a','a','a','a','b','c','c','a','a','d','e','e','e','e'))
// val res21: List[(Int, Char)] = List((4,a), (1,b), (2,c), (2,a), (1,d), (4,e))
def encode[A](list: List[A]): List[(Int, A)] =
  for (gl <- pack(list)) yield (gl.length, gl.head)

// problem 11
def encodeModified[A](list: List[A]): List[(Int, A) | A] =
  for (gl <- pack(list)) yield
    if gl.length == 1 then gl.head else (gl.length, gl.head)

// problem 12
// usage
// scala> decode(List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e')))
// val res5: List[Matchable] = List(a, a, a, a, b, c, c, a, a, d, e, e, e, e)
def decode[A](cl: List[(Int, A) | A]): List[A] =
  if cl.isEmpty then
    List()
  else
    cl.head match
      case (n, i) : (Int, A) => List.fill(n)(i) ++ decode(cl.tail)
      case i : A => List(i) ++ decode(cl.tail)

// problem 13
// scala> l1
// val res7: List[Char] = List(a, a, a, a, b, c, c, a, a, d, e, e, e, e)
//
// scala> encodeDirect(l1)
// val res8: List[(Int, Char) | Char] = List((4,a), b, (2,c), (2,a), d, (4,e))
def encodeDirect[A](l: List[A]): List[(Int, A) | A] =
  if l.isEmpty then
    List()
  else
    val tl = l.takeWhile(_ == l.head)
    val r: (Int, A) | A = if tl.length == 1 then l.head else (tl.length, l.head)
    List(r) ++ encodeDirect(l.drop(tl.length))

// problem 14
// scala> dupli(List('a','b','c','c','d'))
// val res13: List[Char] = List(a, a, b, b, c, c, c, c, d, d)
def dupli[A](list: List[A]): List[A] =
  list.flatMap(a => List.fill(2)(a))

// problem 15
def repli[A](l: List[A], n: Int): List[A] =
  l.flatMap(a => List.fill(n)(a))

// problem 16
// scala> l3
// val res4: List[Char] = List(a, b, c, d, e, f, g, h, i, j)
//
// scala> myDrop(l3, 3)
// val res5: List[Char] = List(a, b, d, e, g, h, j)
def myDrop[A](l: List[A], n: Int): List[A] =
  val o = 
    for 
      i <- 0 until l.length
      if (i+1)%n != 0 
    yield
      l(i)
  o.toList  
 
// problem 17 - do not use any predefined function
// scala> mySplit(l3, 3)
// val res10: List[List[Char]] = List(List(a, b, c), List(d, e, f, g, h, i, j))
def mySplit[A](l: List[A], n: Int): List[List[A]] =
  val x: List[A] = (for i <- 0 until n yield l(i)).toList
  val xs: List[A] = (for i <- n until l.length yield l(i)).toList

  x +: List(xs)

// problem 18 
// scala> mySlice(l3, 3, 7)
// val res11: List[Char] = List(c, d, e, f, g)
def mySlice[A](l: List[A], start: Int, end: Int): List[A] =
  l.take(end).drop(start - 1)

// problem 19
// scala> rotate(l3,-3)
// val res36: List[Char] = List(h, i, j, a, b, c, d, e, f, g)
//
// scala> rotate(l3,-10)
// val res37: List[Char] = List(a, b, c, d, e, f, g, h, i, j)
//
// scala> rotate(l3,2)
// val res38: List[Char] = List(c, d, e, f, g, h, i, j, a, b)
def rotate[A](l: List[A], n: Int): List[A] =
  val nl = if n < 0 then (l.length+n) else n
  val List(fp, np) = mySplit(l, nl)

  np ++ fp

// problem 20
// scala> removeAt(l3, 2)
// val res41: List[Char] = List(a, c, d, e, f, g, h, i, j)
//
// scala> removeAt(l3, 3)
// val res42: List[Char] = List(a, b, d, e, f, g, h, i, j)
def removeAt[A](l: List[A], n: Int): List[A] =
  val List(fp, np) = mySplit(l, n-1)
  fp ++ np.tail

// problem 21
// val res3: List[Matchable | List[Matchable]] = List(a, b, c, d, abc, e, f, g, h)
// scala> insertAt("a", l1, 5)
// val res4: List[Matchable | List[Matchable]] = List(a, b, c, d, a, e, f, g, h)
// scala> insertAt('z', l1, 5)
// val res5: List[Char | List[Char]] = List(a, b, c, d, z, e, f, g, h)
def insertAt[A](item: A | List[A], l: List[A], n: Int): List[A | List[A]] =
  val List(fp, np) = mySplit(l, n-1)
  fp ++ List(item) ++ np

// problem 22
def myRange(start: Int, end: Int): List[Int] =
  Range.inclusive(start, end, (end-start).sign).toList

// problem 23
import scala.util.Random
def rndSelect[A](l: List[A], n: Int): List[A] =
  Random.shuffle(l).take(n)

// problem 24
def lottoSelect(start: Int, end: Int): List[Int] =
  rndSelect(myRange(start, end),Random.between(start, end - start))

// problem 25
def rndPermu[A](l: List[A]): List[A] = Random.shuffle(l)

// problem 26
def myCombination2[A](n: Int, l: List[A]): List[List[A]] = l.combinations(n).toList

// scala> myCombination(2, List('a','b','c','d','e'))
// val res31: List[List[Char]] = List(List(a, b), List(a, c), List(a, d), List(a, e), List(b, c), List(b, d), List(b, e), List(c, d), List(c, e), List(d, e))
def myCombination[A](n: Int, l: List[A]): List[List[A]] = 
  if l.isEmpty then
    List()
  else if n == l.length then
    List(l)
  else if n == 1 then
    l.grouped(1).toList
  else 
    myCombination(n-1, l.tail).map(List(l.head)++_) ++ myCombination(n, l.tail)

def factorial(n: Int): Int =
  if n == 1 || n < 1 then
    1
  else
    n * factorial(n-1)

// problem 27
// In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
// scala> group3(l2)
// val res21: List[List[List[Int]]] = List(List(List(1, 2), List(3, 4, 5), List(6, 7, 8, 9)), List(List(1, 2), List(3, 4, 6), List(5, 7, 8, 9)), List(List(1, 2), List(3, 4, 7), List(5, 6, 8, 9)), List(List(1, 2), List(3, 4, 8), List(5, 6, 7, 9)), List(List(1, 2), List(3, 4, 9), List(5, 6, 7, 8)), List(List(1, 2), List(3, 5, 6), ...
// scala> res21(0)
// val res25: List[List[Int]] = List(List(1, 2), List(3, 4, 5), List(6, 7, 8, 9))
// 
// scala> res21(37)
// val res26: List[List[Int]] = List(List(1, 3), List(2, 4, 7), List(5, 6, 8, 9))
// scala> res21(1000)
// val res29: List[List[Int]] = List(List(5, 7), List(2, 4, 7), List(1, 3, 6, 8, 9))
def group3[A](l: List[A]): List[List[List[A]]] =
  val combiOf2: List[List[A]] = myCombination(2, l) // should have 36 Lists
  val leftOver7: List[List[A]] = combiOf2.map(l.diff(_)) // 36 lists
  val combiOf3: List[List[A]] = leftOver7.flatMap(myCombination(3,_)) // should have 36 * 35
  // final 3 groups are combiOf2 + this 7 + leftover 4
  var counter = 0 // not ideal
  combiOf3.grouped(combiOf2.length).toList.flatMap(ll =>
      val r = ll.map(a=>
        val leftOver4 = leftOver7(counter).diff(a)
        List(combiOf2(counter))++List(a)++List(leftOver4)
      )
      counter += 1
      r
    )


