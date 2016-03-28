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
	*	Exercise 5.13 - map,take,takeWhile,zipWith and zipAll via unfold.
	*
	*	mapUnfold input example:
	*	Stream(1,2,3,4,5,6).mapUnfold(i => i * 10).toList
	*	
	*	Expected Output:
	*	List[Int] = List(10,20,30,40,50,60)
	*
	*	takeUnfold input example:
	*	Stream(1,2,3,4,5,6).takeUnfold(4).toList
	*
	*	Expected Output:
	*	List[Int] = List(1,2,3,4)
	*
	*	takeWhileUnfold input example:
	*	Stream(2,4,6).takeWhileUnfold(i=>i%2==0).toList
	*
	*	Expected Output:
	*	List[Int] = List(2,4,6)
	*
	*	zipWith input example:
	*	Stream(1,2,3,4,5,6).zipWith(Stream(7,8,9,10,11,12))((x,y) => x + y).toList
	*
	*	Expected Output:
	*	List[Int] = List(8,10,12,14,16,18)
	*
	*	zipAll input example:
	*	Stream(1,2).zipAll(Stream(5,6,7)).toList
	*
	*	Expected Output:
	*	List(Option[Int], Option[Int]) = List((Some(1), Some(5), (Some(2), Some(6)), (None, Some(7))))
	*/
	def mapUnfold[B](f: A => B) : Stream[B] = unfold(this) {
		case Cons(h,t) => Some(f(h()),t())
		case _ => None
	}

	def takeUnfold(n: Int) : Stream[A] = unfold((this, n)) {
		case (Cons(h,t), 1) => Some((h(), (empty,0)))
		case (Cons(h,t), i) if i > 1 => Some((h(), (t(), i - 1)))
		case _ => None 
	}

	def takeWhileUnfold(p: A => Boolean) : Stream[A] = unfold(this) {
		case Cons(h,t) if(p(h())) => Some((h(),t()))
		case _ => None
	}

	def zipWith[B,C](s2: Stream[B])(f: (A,B) => C) : Stream[C] = unfold((this,s2)) {
		case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(),t2())))
		case _ => None
	}

	def zipAll[B](s2: Stream[B]) : Stream[(Option[A], Option[B])] = unfold((this,s2)) {
		case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1())),(Some(h2()))),((t1(),t2())))
		case (Cons(h1,t1), Empty) => Some(((Some(h1())), None),((t1(),empty)))
		case (Empty, Cons(h2,t2)) => Some(((None, (Some(h2())))),((empty,t2()))) 
		case _ => None
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

	def unfold[A,S](z: S)(f: S => Option[(A,S)]) : Stream[A] = f(z) match {
		case Some((x,y)) => cons(x,unfold(y)(f))
		case _ => empty
	}
}