import Stream._

sealed trait Stream[+A] {
	/*
	*	Exercise 5.1 - Convert Stream to List.
	*/
	def toList : List[A] = this match {
		case Empty => Nil
		case Cons(h,t) => h() :: t().toList
	}

	/*
	* 	Exercise 5.2 - take(n) returns the first n elements of a Stream.
	*/
	def take(n: Int) : Stream[A] = this match {
		case Cons(h,t) => if(n>0) cons(h(),t().take(n-1)) else empty
		case _ => empty
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