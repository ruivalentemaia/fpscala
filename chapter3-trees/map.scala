sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	
	/*
	*	Exercise 3.28 - Function "map" that modifies each element in a Tree
	*	according to a given function.
	*
	*	Input: val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
	*	Tree.map(t) (i => i * 10)
	*
	*	Expected Output: Branch(Branch(Leaf(10), Leaf(20)), Branch(Leaf(30), Leaf(40)))
	*
	*	Input2: val t2 = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
	*	Tree.map(t2) (i => i.toString)
	*
	*	Expected Output: Branch(Branch(Leaf("1"), Leaf("2")), Branch(Leaf("3"), Leaf("4")))
	*/
	def map[A,B](tree: Tree[A]) (f: A => B) : Tree[B] = tree match {
		case Leaf(x) => Leaf(f(x))Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
		case Branch(l,r) => Branch(map(l)(f),map(r)(f))
	}
}