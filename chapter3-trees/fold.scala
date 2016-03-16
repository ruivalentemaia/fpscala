sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	
	/*
	*	Exercise 3.29 - Function "fold" that generalizes over functions size,
	*	maximum, depth and map.
	*
	*	Input: val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
	*	Tree.fold(t)((x,y) => x max y)
	*/
	def fold[A,B](tree: Tree[A]) (f: A => B) (g: (B,B) => B) : B = tree match {
		case Leaf(x) => f(x)
		case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
	}

	def sizeFold[A](tree: Tree[A]) : Int =
		fold(tree) (_ => 1) (1 + _ + _)

	def maximumFold[A](tree: Tree[Int]) : Int =
		fold(tree) ((v:Int) => v) (_ max _)

	def depthFold[A](tree: Tree[A]) : Int =
		fold(tree) (_=>0) ((j: Int,k: Int) => 1 + (j max k))

	def mapFold[A,B](tree:Tree[A]) (f: A => B) : Tree[B] = 
		fold(tree) ((x: A) => Leaf(f(x)) : Tree[B]) (Branch(_,_))
}