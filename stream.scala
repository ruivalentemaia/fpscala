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
	*	drop(n) skips the first n elements of a Stream.
	*/
	def take(n: Int) : Stream[A] = this match {
		case Cons(h,t) => if(n>0) cons(h(),t().take(n-1)) else empty
		case _ => empty
	}

	def drop(n: Int) : Stream[A] = this match {
		case Cons(h,t) => if(n > 0) t().drop(n-1) else this
		case _ => this
	}

	/*
	*	Exercise 5.3 - takeWhile returns all elements of a Stream that
	*	match a given predicate.
	*/
	def takeWhile(p: A => Boolean) :  Stream[A] = this match {
		case Cons(h,t) => {
			if(p(h())) cons(h(),t().takeWhile(p))
			else t().takeWhile((p))
		}
		case _ => empty
	}

	/*
	*	Exercise 5.4 - forAll checks that all elements in the Stream match
	*	a given predicate and terminates when one doesn't.
	*/
	def forAll(p: A => Boolean) : Boolean = this match {
		case Cons(h,t) => if(p(h())) t().forAll(p) else false
		case _ => true
	}

	/*
	*	Exercise 5.5 - Use foldRight to implement takeWhile
	*
	*	Input: Stream(1,2,3,4,5,6).takeWhileFold(i => i%2 == 0).toList
	*	Output: Stream(2,4,6)
	*/
	def foldRight[B](z: => B)(f: (A, => B) => B) : B = this match {
		case Cons(h,t) => f(h(), t().foldRight(z)(f))
		case _ => z	
	}

	def takeWhileFold(p: A => Boolean) : Stream[A] = 
		foldRight[Stream[A]](empty)((h,t) => if(p(h)) cons(h,t) else t)

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