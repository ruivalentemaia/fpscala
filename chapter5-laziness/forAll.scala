import Stream._

sealed trait Stream[+A] {
	/*
	*	Exercise 5.4 - forAll checks that all elements in the Stream match
	*	a given predicate and terminates when one doesn't.
	*/
	def forAll(p: A => Boolean) : Boolean = this match {
		case Cons(h,t) => if(p(h())) t().forAll(p) else false
		case _ => true
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