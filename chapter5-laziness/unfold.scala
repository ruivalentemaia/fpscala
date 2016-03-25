import Stream._

sealed trait Stream[+A] {

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

	/*
	*	Exercise 5.11 - General stream-building function unfold.
	*/
	def unfold[A,S](z: S)(f: S => Option[(A,S)]) : Stream[A] = f(z) match {
		case Some((x,y)) => cons(x,unfold(y)(f))
		case _ => empty
	}

	/*
	*	Exercise 5.12 - fibs, from, constant and ones in terms of unfold.
	*/
	def onesUnfold : Stream[Int] =
		unfold(1)(i => Some((i,i)))

	def constantUnfold(a: Int) : Stream[Int] =
		unfold(a)(i => Some((i,i)))

	def fromUnfold(n: Int) : Stream[Int] =
		unfold(n)(i => Some((i,i+1)))

	def fibsUnfold: Stream[Int] =
		unfold(0,1)(f => Some(f._1, (f._2,f._1 + f._2)))

}