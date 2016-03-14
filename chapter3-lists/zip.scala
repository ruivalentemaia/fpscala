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
	*	Exercise 3.22 - Function "zip", which accepts two lists of integers
	* 					and constructs a new list by adding corresponding
	*					elements.
	*
	*	Input: List.zip(List(1,2,3), List(4,5,6))
	*	Expected Output: List[Int] = List(5,7,9)
	*/

	def zip(ls: List[Int], ns: List[Int]) : List[Int] = (ls,ns) match {
		case (_,Nil) => ls
		case (Nil,_) => ns
		case (Cons(x,xs),Cons(h,t)) => Cons(x + h, zip(xs,t))
	}
}