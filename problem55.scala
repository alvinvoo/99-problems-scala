class Tree[A](data: A, left: Tree[A] = null, right: Tree[A] = null):
  override def toString = s"|${left}<-${data}->${right}|"

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



