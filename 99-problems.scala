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
    




