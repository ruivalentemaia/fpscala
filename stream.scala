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

	/*
	*	Exercise 5.6 - Implement headOption via foldRight.
	*
	*	Input: Stream(1,2,3,4,5,6).headOption
	*	Output: Some(1)
	*/
	def headOptionViaFold : Option[A] =
		foldRight[Option[A]](None)((a,_) => Some(a))

	/*
	*	Exercise 5.7 - Implement map, filter, append and flatMap using foldRight.
	*/
	def map[B](f: A => B) : Stream[B] = 
		foldRight(empty[B])((a,b) => cons(f(a),b))

	def filter(f: A => Boolean) : Stream[A] =
		foldRight[Stream[A]](empty)((a,b) => if(f(a)) cons(a,b) else b)

	def append[B >: A](a: => Stream[B]) : Stream[B] =
		foldRight(a)((h,t) => cons(h,t))

	def flatMap[B](f: A => Stream[B]) : Stream[B] =
		foldRight(empty[B])((h,t) => f(h) append t)

	def find(p: A => Boolean) : Option[A] =
		filter(p).headOptionViaFold

	/*
	*	Exercise 5.13 - map,take,takeWhile,zipWith and zipAll via unfold.
	*/
	def mapUnfold[B](f: A => B) : Stream[B] = 
		unfold(this)(i => Some((f(i),i)))

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