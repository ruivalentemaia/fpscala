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
	*	Exercise 3.24 - Function "hasSubsequence", which checks whether a List
	*					contains another List as a subsequence.
	*
	*	Input 1: List.hasSubsequence(List(1,2,3,4),List(1,2))
	*	Expected Output 1: True
	*
	*	Input 2: List.hasSubsequence(List(1,2,3,4), List(1,5))
	* 	Expected Output 2: False
	*/
	def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = (sup,sub) match {
		case (Nil,_) => false
		case (_,Nil) => false
		case (Cons(x,xs), Cons(h,Nil)) => {
			if(x == h) true
			else hasSubsequence(xs,sub)
		}
		case (Cons(x,xs), Cons(h,t)) => {
			if(x == h) hasSubsequence(xs,t)
			else hasSubsequence(sup, t)
		}
	}
}