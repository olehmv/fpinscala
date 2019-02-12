// Algebraic data types can be used to define other data structures.
// Let’s define a simpl binary tree data structure:

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//EXERCISE 3.25
def size[A](tree: Tree[A]): Int = {
  tree match {
    case Leaf(v) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }
}
val treeOfDepth3 = Branch(Branch(Leaf(8), Leaf(22)), Branch(Leaf(3), Branch(Leaf(34), Leaf(4))))
val treeOfDepth4 = Branch(Branch(Leaf(8), Leaf(22)), Branch(Leaf(3), Branch(Leaf(34), Branch(Leaf(34), Leaf(4)))))
val leftTreeOfDepth3rightTreeOfDepth4 = Branch(treeOfDepth3, treeOfDepth4)
size(treeOfDepth3)
size(treeOfDepth4)
size(leftTreeOfDepth3rightTreeOfDepth4)

//EXERCISE 3.26
def maximum(tree: Tree[Int]): Int = {
  tree match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max (maximum(right))
  }
}
maximum(treeOfDepth3)

//EXERCISE 3.27 maximum path depth
def depth[A](tree: Tree[A]): Int = {
  tree match {
    //A root node have a depth of 0
    case Leaf(v) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }
}
depth(treeOfDepth3)
depth(treeOfDepth4)
println(leftTreeOfDepth3rightTreeOfDepth4)
depth(leftTreeOfDepth3rightTreeOfDepth4)

//EXERCISE 3.28
def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
map(treeOfDepth3)(i => i.toDouble)

//EXERCISE 3.29
def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
  case Leaf(a) => f(a)
  case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 1)(1 + _ + _)

def maximumViaFold(t: Tree[Int]): Int =
  fold(t)(a => a)(_ max _)

def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))




