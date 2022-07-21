import scala.math.*, scala.collection.mutable.*
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
// scala> res21(1000) <-- wrong
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

def listProduct[A](l1: List[A], l2: List[A]) =
  for a1 <- l1
      a2 <- l2
  yield
      List(a1) ++ List(a2)
      
// problem 27b
// scala> myGroup(l2,List(2,3,4))
// val res34: List[List[List[Char]]] = List(List(List(a, b), List(c, d, e), List(f, g, h, i)), List(List(a, b), List(c, d, f), List(e, g, h, i)), List(List(a, b), List(c,       
// scala> res34(1000)
// val res35: List[List[Char]] = List(List(e, h), List(b, d, g), List(a, c, f, i))
def myGroup[A](l: List[A], sets: List[Int]): List[List[List[A]]] =
  if sets.isEmpty then
    List(List())
  else
    val curN = sets.head
    myCombination(curN, l).flatMap(c=>
        myGroup(l.diff(c), sets.tail).map(a =>
          List(c) ++ a
        ))
// problem 28a - length sort
// scala> lsort(List(List('a','b','c'), List('d','e'), List('f','g','h'), List('d','e'), List('i','j','k','l'), List('m','n'), List('o')))
// val res3: List[List[Char]] = List(List(o), List(d, e), List(d, e), List(m, n), List(a, b, c), List(f, g, h), List(i, j, k, l))
def lsort[A](l: List[List[A]]): List[List[A]] =
  l.sorted(Ordering.by[List[A], Int](_.length)) 
    
// problem 28b - actually problem 30
def lfsort[A](l: List[List[A]]): List[List[A]] =
  val hm = l.groupBy(_.length).map((k,v) => k -> (v.length, v)).iterator.toList
  val sortedHm = hm.sorted(Ordering.by[(Int, (Int, List[List[A]])), Int](_._2._1))
  sortedHm.flatMap(_._2._2)


// problem 31
// https://byjus.com/maths/how-to-find-prime-numbers/#How%20to%20check%20a%20prime%20number?
// https://socratic.org/questions/how-are-prime-numbers-used
def sumOfDigits(n: Int): Int =
  if n == 0 then
    0
  else 
    val (div, rem) = (BigInt(n)/%BigInt(10))
    rem.intValue + sumOfDigits(div.intValue)
    
// scala> genPrime(200)
// val res54: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199)
def genPrime(n: Int): List[Int] = 
  var primes = ArrayBuffer[Int](2)
  for 
    i <- 3 to n 
  do
    if primes.forall(i%_ != 0) then primes.addOne(i)
  primes.toList
   
// problem 31
def isPrime(n: Int): Boolean =
  val lastDigit = (BigInt(n)/%BigInt(10))._2.intValue
  // false if unit place ends with 0,2,4,6,8
  if List(0,2,4,6,8).foldLeft(false)((a,b) => b == lastDigit || a) then
    false
  // If a large number is ending with 5, then it is always divisible by 5.
  else if n > 10 && lastDigit == 5 then
    false
  // If sum of digits divisible by 3, then not prime number
  else if sumOfDigits(n) % 3 == 0 then
    false
  else
    genPrime(sqrt(n).toInt).forall(n%_!=0)

// problem 35
def factorize(n: Int): List[Int] =
  val factors = ArrayBuffer[Int]()
  val uniquePrimes = genPrime(n).filter(n%_==0)
  var tempN = n
  for u <- uniquePrimes do
    while tempN % u == 0 do
      factors.addOne(u)
      tempN/=u
  factors.toList 
  
// problem 32
// https://byjus.com/maths/greatest-common-divisor/
// scala> gcd(36, 63)
// val res92: Int = 9
def gcd(n1: Int, n2: Int): Int =
  val commonFactors = factorize(n1).intersect(factorize(n2))
  if commonFactors.length == 0 then 1
  else commonFactors.reduce(_ * _)

// problem 33
def coprime(n1: Int, n2: Int): Boolean =
  gcd(n1,n2) == 1

// problem 34
// Euler's so-called totient function phi(m) is defined as the NUMBER (size) of positive integers r (1 <= r < m) that are coprime to m.
//  Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
//  For the example, there are FOUR coprimes for 10 (less than 10, excluding itself)
def totientPhi(n: Int): Int =
  var totientSize = 0
  for i <- 1 until n do
      if coprime(i, n) then totientSize += 1
  totientSize


