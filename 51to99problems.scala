import scala.math.*
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

import math.Ordered.orderingToOrdered
// problem 55
// better have a generic Tree class 
class Tree[A](val data: A, val left: Tree[A] = null, val right: Tree[A] = null):
  def swap(): Tree[A] =
    Tree(data, right, left)

  def isNodeLeaf(): Boolean =
    left == null && right == null

  override def toString = s"|${left}<-${data}->${right}|"
end Tree

object Tree:
 def clone[A](x: Tree[A]): Tree[A] =
   if x == null then null
   else if x.isNodeLeaf() then Tree(x.data, null, null)
   else Tree(x.data, Tree.clone(x.left), Tree.clone(x.right))
 def toList[A](x: Tree[A]): List[Any] =
   if x == null then List()
   else if x.isNodeLeaf() then List(x.data, null, null)
   else List(x.data, Tree.toList(x.left), Tree.toList(x.right))
 def bstInsert[A: Ordering](x: Tree[A] = null, key: A): Tree[A] =
    if x == null then Tree(key)
    else 
      if key < x.data then
        Tree(x.data, Tree.bstInsert(x.left, key), x.right)
      else
        Tree(x.data, x.left, Tree.bstInsert(x.right, key))

// the number of variations would be 2^(#longest valid depth)
// reuse previous
def listProduct[A](l1: List[A], l2: List[A]) =
  for a1 <- l1
      a2 <- l2
  yield
      List(a1) ++ List(a2)

def genTreeProducts[A](data: A, l1: List[Tree[A]], l2: List[Tree[A]]): List[Tree[A]] =
  listProduct(l1, l2).map(a =>
      val List(t1, t2) = a
      Tree(data, t1, t2)
      )

def cBalTree[A](data: A, n: Int): List[Tree[A]] =
  n match
  case n if n <= 0 => List(null)
  case n if n % 2 == 0 => 
    genTreeProducts(data, cBalTree(data, (n-2)/2), cBalTree(data, n/2)) ++ genTreeProducts(data, cBalTree(data, n/2), cBalTree(data, (n-2)/2))
  case _ => 
    genTreeProducts(data, cBalTree(data, (n-1)/2), cBalTree(data, (n-1)/2))

def cBalTreePrint[A](data: A, n: Int): Unit =
  cBalTree(data, n).foreach(println)
// problem 56
// scala> val t = Tree('A',Tree('A',null,Tree('A', null, null)), Tree('A',Tree('B',null, null),null))
// val t: Tree[Char] = ||null<-A->|null<-A->null||<-A->||null<-B->null|<-A->null||
//
// scala> isTreeSymmetrical(t)
// val res23: Boolean = true
def convertToListWEmptyData[A](x: Tree[A]): List[Any] =
   if x == null then List()
   else if x.isNodeLeaf() then List(0, null, null)
   else List(0, convertToListWEmptyData(x.left), convertToListWEmptyData(x.right))

def isTreeSymmetrical[A](tree: Tree[A]): Boolean = 
  if tree == null || tree.isNodeLeaf() then true
  else
    val tll = convertToListWEmptyData(tree.left.swap())
    val tr = convertToListWEmptyData(tree.right)
    tll == tr

// problem 57 - BST 
def bstConstruct(l: List[Int]): Tree[Int] = 
  l.tail.foldLeft(Tree(l.head))(Tree.bstInsert(_, _))

//scala> bstSymmetric(List(5,3,18,1,4,12,21))
// val res42: Boolean = true
// scala> bstSymmetric(List(3,2,5,7))
// val res43: Boolean = false
def bstSymmetric(l: List[Int]): Boolean =
  isTreeSymmetrical(bstConstruct(l))



