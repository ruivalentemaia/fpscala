sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

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