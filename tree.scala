sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
	
	/*
	*	Exercise 3.25 - Function "size" which counts the number of
	*	nodes (branches and leaves) in a tree.
	*
	*	Input: 
	*	val t = Branch(Branch(Leaf("a"), Leaf("b")),
	*					Branch(Leaf("c"), Leaf("d")))
	*	Tree.size(t);
	*	
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
	*	Int in a Tree.
	*	
	*	Input: val t = Branch(Branch(Leaf(4),Leaf(20)), Branch(Leaf(8), Leaf(18)))
	*	Tree.maximum(t)
	*
	*	Expected Output: 20.
	*/
	def maximum(tree: Tree[Int]) : Int = tree match{
		case Leaf(x) => maximum(Leaf(x))
		case Branch(Leaf(y),Leaf(x)) => x max y
		case Branch(l,r) => maximum(l) max maximum(r)
	}

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