import Stream._

sealed trait Stream[+A] {
	/*
	*	Exercise 5.1 - Convert Stream to List.
	*/
	def toList : List[A] = this match {
		case Empty => Nil
		case Cons(h,t) => h() :: t().toList
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