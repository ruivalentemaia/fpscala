import Stream._

sealed trait Stream[+A] {
	/*
	*	Exercise 5.1 - Convert Stream to List.
	*/
	def toList : List[A] = this match {
		case Empty => Nil
		case Cons(h,t) => h() :: t().toList
	}

	def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z	
	}

	/*
	*	Exercise 5.7 - Implement map, filter, append and flatMap using foldRight.
	*/
	def map[B](f: A => B) : Stream[B] = 
		foldRight(empty[B])((a,b) => cons(f(a),b))

	def filter(f: A => Boolean) : Stream[A] =
		foldRight[Stream[A]](empty)((a,b) => if(f(a)) cons(a,b) else b)

	def append[B >: A](a: => Stream[B]) : Stream[B] = {
		foldRight(a)((h,t) => cons(h,t))
	}
	def flatMap[B](f: A => Stream[B]) : Stream[B] = {
		foldRight(empty[B])((h,t) => f(h) append t)
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