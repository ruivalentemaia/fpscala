import Stream._

sealed trait Stream[+A] {
	
	def toList : List[A] = this match {
		case Empty => Nil
		case Cons(h,t) => h() :: t().toList
	}

	/*
	*	Exercise 5.8 - Returns infinite Stream of a given value.
	*/
	def constant[A](a: A) : Stream[A] =
		Stream.cons(a,constant(a))

	/*
	*	Exercise 5.9 -  Generates infinite Stream of integers, starting from n
	*	then n+1, n+2 and so on.
	*/
	def from(n: Int) : Stream[Int] = 
		Stream.cons(n,from(n+1))

	/*
	*	Exercise 5.10 - Generates infinite Stream of Fibonacci numbers.
	*/
	def fibs(prev: Int, next: Int) : Stream[Int] = {
		Stream.cons(prev,fibs(next,prev+next))
	}

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, tl: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A] : Stream[A] = Empty

	def apply[A](as: A*) : Stream[A] = 
		if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}