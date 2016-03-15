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
	*	Exercise 3.23 - Function "zipWith", that performs what Exercise 3.22
	*					zip function does, but it's not specific to integers
	*					or addition.
	*
	*	Input: def subLists(ls: List[Double], ns: List[Double]) : List[Double] =
	*				List.zipWith(ls,ns)((x,y) => x - y)
	*			subLists(List(5.0,5.0,6.0),List(4.0,4.0,5.0))
	*	Expected Output: List[Double] = List(1.0,1.0,1.0)
	*/
	def zipWith[A,B,C](ls: List[A], ns: List[B]) (f: (A,B) => C) : List[C] = (ls,ns) match {
		case (_,Nil) => Nil
		case (Nil,_) => Nil
		case (Cons(x,xs),Cons(h,t)) => Cons(f(x,h), zipWith(xs,t)(f))
	}
}