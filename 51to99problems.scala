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
    val tll = convertToListWEmptyData(if tree.left == null then null else tree.left.swap())
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

// problem 58
def symCbalTrees(n: Int): List[Tree[Char]] =
  cBalTree('A', n).filter(isTreeSymmetrical)

def symBalTreesPrint() =  
  for i <- Range(1,58) do
    println(s"${i} : ${symCbalTrees(i).length}")
/**
 *
1 : 1
2 : 0
3 : 1
4 : 0
5 : 2
6 : 0
7 : 1
8 : 0
9 : 4
10 : 0
11 : 4
12 : 0
13 : 4
14 : 0
15 : 1
16 : 0
17 : 8
18 : 0
19 : 16
20 : 0
21 : 32
22 : 0
23 : 16
24 : 0
25 : 32
26 : 0
27 : 16
28 : 0
29 : 8
30 : 0
31 : 1
32 : 0
33 : 16
34 : 0
35 : 64
36 : 0
37 : 256
38 : 0
39 : 256
40 : 0
41 : 1024
42 : 0
43 : 1024
44 : 0
45 : 1024
46 : 0
47 : 256
48 : 0
49 : 1024
50 : 0
51 : 1024
52 : 0
53 : 1024
54 : 0
55 : 256
56 : 0
57 : 256
 */

// for one roughly balanced tree only
// with node counts as data instead
def cBalTreeOne(n: Int): Tree[Int] =
  n match
  case n if n <= 0 => null
  case 1 => Tree(1)
  case _ => 
    Tree(n, cBalTreeOne(math.ceil((n-1)/2.0).toInt), cBalTreeOne(math.floor((n-1)/2.0).toInt))

def collectNodes(t: Tree[Int]): List[(Int, Int)] =
  if t == null then List((0,0))
  else
    val tl = if t.left != null then t.left.data else 0
    val tr = if t.right!= null then t.right.data else 0
    List((tl, tr)) ++ collectNodes(t.left) ++ collectNodes(t.right)

// formula for calculating balance symmetry variations
// as long as for a root-tree its two sub-trees are not symmetrical (i.e. the node count is not the same), they can be mirrored, hence a variation of 2
// dot product (or just multiply) all these variations of every nodes but divided by 2 (becoz the main root-sub-trees should already be symmetrical)
def calcVariations(n: Int): Int =
  val pow2 = collectNodes(cBalTreeOne(n)).filter(_!=_).length / 2
  math.pow(2, pow2).toInt

// problem 58 
// take a Tree and count its balance symmetry variants based
// on a simplified function
def countSymBalTree(n: Int): Int = 
  // IF n is even, then its 0 else calculate the variations
  if n % 2 == 0 then 0
  else calcVariations(n)

// problem 59
// similar to problem 55 - complete balance tree
// but instead recurse on Height instead, look at case when h = 2
// each result of h values depends on the building blocks of previous h values
def hbalTree[A](data: A = 'X', h: Int): List[Tree[A]] =
    if h == 0 then List(null) 
    else if h == 1 then List(Tree(data))
    else
      genTreeProducts(data, hbalTree(data, h-1), hbalTree(data, h-1)) ++
      genTreeProducts(data, hbalTree(data, h-1), hbalTree(data, h-2)) ++
      genTreeProducts(data, hbalTree(data, h-2), hbalTree(data, h-1)) 



