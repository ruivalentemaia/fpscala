sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	
	/*
	*	Exercise 3.27 - Function "depth" that returns the maximum path
	*	length from the root of a tree to any leaf.
	*
	*	Input: val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)),
	*	Branch(Leaf(5), Leaf(6))))
	*	Tree.depth(t)
	*	
	*	Expected Output: 4
	*
	*	Input2: val t2 = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), 
	*	Branch(Branch(Leaf(4), Branch(Leaf(5),Leaf(6))), Branch(Leaf(7), Leaf(8))))
	*	Tree.depth(t2)
	*
	*	Expected Output: 5
	*/
	def depth[A](tree: Tree[A]) : Int = {
		def getDepth(tree: Tree[A], n: Int) : Int = tree match {
			case Leaf(x) => n
			case Branch(l,r) => getDepth(l,n+1) max getDepth(r,n+1)
		} 
		getDepth(tree,1)
	}
}