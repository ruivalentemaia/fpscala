sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	
	/*
	*	Exercise 3.25 - Function "size" which counts the number of
	*					nodes (branches and leaves) in a tree.
	*
	*	Input: val t = Branch(Branch(Leaf("a"), Leaf("b")),
	*						  Branch(Leaf("c"), Leaf("d")))
	*			Tree.size(t);
	*	Expected Output: 7.
	*/
	def size[A](tree: Tree[A]) : Int = {
		def getSize(tree: Tree[A], n: Int): Int = tree match {
			case Leaf(_) => n + 1
			case Branch(l,r) => 1 + getSize(l,n) + getSize(r,n) 
		}
		getSize(tree,0)
	}

	/*
	*	Exercise 3.26 - Function "maximum" that computes the maximum
	*					Int in a Tree.
	*	Input: val t = Branch(Branch(Leaf(4),Leaf(20)), Branch(Leaf(8), Leaf(18)))
	*		   Tree.maximum(t)
	*	Expected Output: 20.
	*/
	def maximum(tree: Tree[Int]) : Int = {
		def getMax(tree: Tree[Int], x: Int) : Int = tree match {
			case Leaf(y) => x max y
			case Branch(l,r) => getMax(l,x) max getMax(r,x)
		}
		getMax(tree,-1000000)
	}
}