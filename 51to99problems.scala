// problem 54A
// check whether a list represent a binary tree
// scala> isTree(List(1, List(2, null, null)))
// val res10: Boolean = false
// scala> isTree(List(1, List(2, null, null), null))
// val res11: Boolean = true
// scala> isTree(List(1, List(2, null, null), List(5, List(6, null, null), null)))
// val res9: Boolean = true
def isTree[A](l: List[A]): Boolean =
  l match 
    case List() => true
    case List(_:A, null, null) => true
    case List(_:A, b: List[A], c: List[A]) => isTree(b) & isTree(c)
    case List(_:A, b: List[A], null)  => isTree(b)
    case _ => false

// problem 55
// better have a generic Tree class 



