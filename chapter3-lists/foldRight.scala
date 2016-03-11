sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds:List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0,_) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))

	def tail[A](l: List[A]) : List[A] = l match {
		case Nil => l
		case Cons(x,xs) => xs
	}

	/*
	* Exercise 3.7 - Product Halt, implemented using foldRight,
	* immediately halt the recursion and return 0.0 if it encounters
	* a 0.0 ?
	*	Answer: according to my implementation, yes.
	*
	* Test Case:
	*	1)
	*	val x = List(1.0,2.0,3.0,4.0,0.0,5.0,6.0)
	*	List.productHalt(x)
	*
	* 	Expected Output: 0.0
	*
	*	2)
	*	val x = List(1.0,2.0,5.0)
	*	List.productHalt(x)
	*
	*	Expected Output: 10.0	
	*/
	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
		as match {
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs, z)(f))
		}

	def sumFold(ns: List[Int]) =
		foldRight(ns, 0)((x,y) => x + y)


	def productFold(ns:List[Double]) =
		foldRight(ns, 1.0)((x,y) => x*y)

	def productHalt(ns:List[Double]) =
		ns match {
			case Nil => foldRight(Nil,1.0)((x,y) => x)
			case Cons(h,t) => {
				if(h == 0.0) foldRight(Nil, 0.0)((x,y) => x)
				else foldRight(ns,1.0)((x,y) => x*y)
			}
		}
}