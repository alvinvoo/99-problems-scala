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

// this one is the cartesion produce between 2 lists
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
def primeFactor(n: Int): List[Int] =
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
  val commonFactors = primeFactor(n1).intersect(primeFactor(n2))
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

// problem 35
// see primeFactor above
//

// problem 36
def primeFactorsMult(n: Int): List[(Int, Int)] =
  encode(primeFactor(n)).map(t => 
      val (a,b) = t
      (b,a)
      )

// problem 37
// m = 10: primeFactors = 2,5
// primeFactorMult = ((2 1) (5 1))
// formula: phi(m) = (p1 - 1) * p1 ** (m1 - 1) * (p2 - 1) * p2 ** (m2 - 1) * (p3 - 1) * p3 ** (m3 - 1) * ..
// phi(m) = ((2 - 1)* 2 ** 0) * ((5 - 1)*5 ** 0) 
//        = 1 * 1 * 4 * 1 = 4
def phiImproved(n: Int): Int =
  // primeFactorsMult(n).fold(1)((acc: Int, t: (Int, Int)) =>
  //   val (v,m) = t
  //   acc * (v - 1) * pow(v, (m - 1) 
  //   )
  var acc = 1
  for pf <- primeFactorsMult(n) do
    val (v,m) = pf
    acc *= ((v - 1) * pow(v, (m - 1)).toInt)
  acc

// problem 38
// while p#34 totientPhi(n) needs to do gcd (which make use of primeFactor twice) with O(n)
// p#37 do primeFactor ONCE and do O(#pf) calculations only
// scala> def time[A](f: => A) = {
//      |   val s = System.nanoTime
//      |   val ret = f
//      |   println("time: "+(System.nanoTime-s)/1e6+"ms")
//      |   ret
//      | }
// def time[A](f: => A): A
//
// scala> time { 10 * 2 }
// time: 0.219932ms
// val res14: Int = 20
//
// scala> time(totientPhi(315))
// time: 43.583117ms
// val res15: Int = 144
//
// scala> time(phiImproved(315))
// time: 0.418312ms // 100x faster
// val res16: Int = 144

// problem 39
def primeList(l: Int, u: Int): List[Int] =
  genPrime(u).dropWhile(_<l)

// problem 40
// Goldbach's conjecture
// Every positive even number greater than 2 is the sum of two prime numbers
// 28 = (5 23)
def goldbach(n: Int): List[(Int, Int)] =
  if n <= 2 || n % 2 != 0 then
    List()
  else
    val primes = genPrime(n)
    val primesDiff = primes.map(n-_) // should be an even length list
    val pl = primes.intersect(primesDiff)
    val (fh, sh) = pl.splitAt(pl.length/2)
    fh.zip(sh.reverse)

def goldbachOne(n: Int): (Int, Int) =
  if n <= 2 || n % 2 != 0 then
    (-1, -1)
  else
    val primes = genPrime(n)
    val primesDiff = primes.map(n-_) // should be an even length list
    val sh = primesDiff.dropWhile(!primes.contains(_)).head
    (n-sh, sh)

// problem 41 - both parts
// scala> goldbachList(1, 2000, 50)
// val res23: collection.mutable.Map[Int, (Int, Int)] = HashMap(992 -> (73,919), 1856 -> (67,1789), 1382 -> (61,1321), 1928 -> (61,1867))
def goldbachList(l: Int, u: Int, limit: Int = 1): Map[Int, (Int, Int)] =
  var retMap: Map[Int, (Int, Int)] = Map()
  for i <- Range.inclusive(l, u) 
      if i % 2 == 0
  do
    val (f, s) = goldbachOne(i)
    if f > limit && s > limit then
      retMap += (i -> (f, s)) 
  retMap

// problem 46 - truth tables
def and(a: Boolean, b: Boolean): Boolean =
  a & b
def or(a: Boolean, b: Boolean): Boolean =
  a | b
def nand(a: Boolean, b: Boolean): Boolean =
  !(a & b)
def nor(a: Boolean, b: Boolean): Boolean =
  !(a | b)
// exactly one input must be true for the output to be true.
def xor(a: Boolean, b: Boolean): Boolean =
  a != b

// default is 2^2 table
// scala> genAB()
// val res25: List[List[Boolean]] = List(List(true, true), List(true, false), List(false, true), List(false, false))
def genAB(n: Int = 2): List[List[Boolean]] =
  if n == 2 then
    List(List(true, true), List(true, false), List(false, true), List(false, false))
  else
    genAB(n-1).flatMap(a =>
        List(a ++ List(true), a ++ List(false))
        )

// scala> truthTableAB((a: Boolean, b: Boolean) => and(a, or(a, b)))
// val res34: List[List[Boolean]] = List(List(true, true, true), List(true, false, true), List(false, true, false), List(false, false, false))
def truthTableAB(f: (Boolean, Boolean) => Boolean): List[List[Boolean]] =
  genAB().map(a =>
      val List(v1, v2) = a
      List(v1, v2, f(v1, v2))
      )

// problem 47
// can extend native Boolean class?
// scala> true andI false // can infix like this
// val res35: Boolean = false
extension(a: Boolean)
  def andI(b: Boolean): Boolean = a & b
  def orI(b: Boolean): Boolean = a | b
  def nandI(b: Boolean): Boolean = !(a & b)
  def norI(b: Boolean): Boolean = !(a | b)
  // exactly one input must be true for the output to be true.
  def xorI(b: Boolean): Boolean = a != b
  def equI(b: Boolean): Boolean = a == b

// scala> truthTableAB((a: Boolean, b: Boolean) => a andI (a orI b))
// val res38: List[List[Boolean]] = List(List(true, true, true), List(true, false, true), List(false, true, false), List(false, false, false))

// problem 48
// since number of parameters are not known before hand. Need to use currying?
// HOF with varargs as parameter doesnt seem to work
// scala> gf
// val res39: (Boolean, Boolean, Boolean) => Boolean = Lambda$2209/1053484140@21ee07da
//
// scala> val f: (Boolean*) => Boolean = gf
// def truthTable(n: Int = 2, f: Boolean* => Boolean): List[List[Boolean]] =
//   genMap(n).map(a =>
//       a ++ List(f(a:_*))
//       )
// macro MIGHT work.. will look later

// problem 49
def gray(n: Int): List[String] =
  if n <= 1 then
    List("0", "1")
  else
    gray(n-1).flatMap(a =>
        List("0", "1").map(_++a) 
        )
// scala> gray(1)
// val res0: List[String] = List(0, 1)
// scala> gray(2)
// val res1: List[String] = List(00, 10, 01, 11)
// scala> gray(3)
// val res2: List[String] = List(000, 100, 010, 110, 001, 101, 011, 111)

// problem 50
// huffman code
// referred from: https://www.programiz.com/dsa/huffman-coding 
type Node = NodeTree | String
class NodeTree(l: Node, r: Node):
  def children() = (l, r) 
  override def toString() =
    s"${l}_${r}"
end NodeTree

def huffman_code(node: Node, binString: String =""): Map[String, String] =
  node match 
    case _: String => Map(node.asInstanceOf[String] -> binString)
    case _ => 
      val (l, r) = node.asInstanceOf[NodeTree].children()
      huffman_code(l, binString+"0") ++ huffman_code(r, binString+"1")

def huffman(f: List[(String, Int)]): Map[String, String] =
  var lb: List[(Node, Int)] = f 
  // first create the tree
  while lb.length > 1 do
    lb = lb.sortWith((a,b) => a._2 < b._2)
    val List(l, r) = lb.take(2)
    lb = lb.drop(2) :+ (NodeTree(l._1, r._1), l._2 + r._2)
  // traverse the tree the get the code for each node leaf
  huffman_code(lb(0)._1)
    

