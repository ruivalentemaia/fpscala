sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}

	def product(ds:List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0,_) => 0.0
		case Cons(x,xs) => x * product(xs)
	}

	def apply[A](as: A*): List[A] =
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))

	def tail[A](l: List[A]) : List[A] = l match {
		case Nil => l
		case Cons(x,xs) => xs
	}

	/*
	*	Exercise 3.12 - Reverses a list.
	*/
	def reverse[A](ls: List[A]) : List[A] = {
		@annotation.tailrec
		def go(ls: List[A], ns: List[A]) : List[A] = 
			ls match {
				case Nil => Nil
				case Cons(x,Nil) => Cons(x,ns)
				case Cons(x,xs) => go(xs, Cons(x,ns))
			}
		if(ls == Nil) Nil
		else go(ls, List[A]())
	}
}