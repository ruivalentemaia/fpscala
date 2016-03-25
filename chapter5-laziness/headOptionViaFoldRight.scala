import Stream._

sealed trait Stream[+A] {

	def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z	
	}

	/*
	*	Exercise 5.6 - Implement headOption via foldRight.
	*/
	def headOptionViaFold : Option[A] =
		foldRight[Option[A]](None)((a,_) => Some(a))

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