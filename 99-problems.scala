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
// val res10: List[List[Char]] = List(List(a, b, c, d), List(d, e, f, g, h, i, j))
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



