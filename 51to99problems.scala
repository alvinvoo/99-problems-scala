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

// problem 55
// better have a generic Tree class 
class Tree[A](var data: A, var left: Tree[A], var right: Tree[A]):
  def updateDataInPlace(data: A) =
    this.data = data

  def swapInPlace() = 
    val tt = left
    this.left = right
    this.right = tt

  def swapInPlaceWithDirs(l: List[String]): Boolean =
    var tt = this
    for ll <- l do 
      ll match
      case "left" | "l" => tt = tt.left
      case "right" | "r" => tt = tt.right
    // if this is a node leaf, then nothing to swap
    // return false
    // println(s"tt is ${tt}")
    if tt == null || tt.isNodeLeaf()
    then 
      false
    else 
      tt.swapInPlace()
      true

  def isNodeLeaf(): Boolean =
    left == null && right == null
    
  override def toString = s"|${left}<-${data}->${right}|"
end Tree

object Tree:
 def clone[A](x: Tree[A]): Tree[A] =
   if x == null then null
   else if x.isNodeLeaf() then Tree(x.data, null, null)
   else Tree(x.data, Tree.clone(x.left), Tree.clone(x.right))

def cBalTreeOne[A](data: A, n: Int): Tree[A] =
  if n < 1 then null
  else if n == 1 then Tree(data, null, null)
  else
    val nn = n - 1
    var f1 = floor; var f2 = ceil
    Tree(data, cBalTreeOne(data, f1(nn/2.0).toInt), cBalTreeOne(data, f2(nn/2.0).toInt))

// the number of variations would be 2^(#longest valid depth)
def cBalTreePrintRest[A](tog: Tree[A], dir: List[String] = List()): Unit =
    val tclone = Tree.clone(tog)
    if tclone.swapInPlaceWithDirs(dir) then
      // if previous dir encounters a node leaf then
      // nothing to print/proceed
      println(tclone)
      cBalTreePrintRest(tog, dir :+ "l")
      cBalTreePrintRest(tclone, dir :+ "l")
      cBalTreePrintRest(tog, dir :+ "r")
      cBalTreePrintRest(tclone, dir :+ "r")
  
// have duplicates - coz the clone flipped might attempt back the flip
def cBalTreePrint[A](data: A, n: Int): Unit =
  val t = cBalTreeOne(data, n)
  println(s"Original tree: ${t}")
  cBalTreePrintRest(t)

  
