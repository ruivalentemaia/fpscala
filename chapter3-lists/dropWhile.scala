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

	def drop[A](l:List[A], n: Int) : List[A] = {
		@annotation.tailrec
		def go(nl: List[A], n:Int) : List[A] = {
			if(n > 0) go(List.tail(nl),n-1)
			else nl
		}
		if (l == Nil) l
		else go(l,n)
	}

	/*
	* Exercise 3.5 - Removes elements from the List
	* prefix as long as they match a predicate.
	*
	* Test case:
	* 	val x = List(2,4,6,7,8,9)
	*	val f = (i:Int) => i % 2 == 0
	*	List.dropWhile(x,f)
	*
	* Expected Output:
	*	List[Int] = Cons(7,Cons(8,Cons(9,Nil)))
	*/
	def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = {
		@annotation.tailrec
		def head(nl: List[A]) : A = nl match {
			case Nil => head(nl)
			case Cons(x,xs) => x
		}

		@annotation.tailrec
		def go(nl: List[A], f: A => Boolean) : List[A] = {
			if(f(head(nl))) go(drop(nl, 1), f)
			else nl
		}

		if(l == Nil) l
		else go(l,f)
	}
}