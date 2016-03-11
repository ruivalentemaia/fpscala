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

	def appendFold[A](ls: List[A], z:A) : List[A] = ls match{
		case Nil => List[A](z)
		case Cons(Nil,xs) => Cons(Nil,appendFold(xs,z))
		case Cons(x,Nil) => List[A](x,z)
		case Cons(x,xs) => Cons(x,appendFold(xs,z))
	}

	/*
	*	Exercise 3.19 - Function filter, that removes elements from a list
	* 					unless they satisfy a given predicate.
	*/
	def filter[A](as: List[A])(f: A => Boolean) : List[A] = {
		@annotation.tailrec
		def filterThem(as: List[A], ns: List[A]) (f: A => Boolean) : List[A] = as match {
			case Nil => ns
			case Cons(Nil, xs) => filterThem(xs,ns)(f)
			case Cons(x,Nil) => {
				if(!f(x)) {
					drop(as,1)
					filterThem(Nil,ns)(f)
				}
				else {
					filterThem(Nil, appendFold(ns,x))(f)
				}
			}
			case Cons(x,xs) => {
				if(!f(x)) {
					drop(as,1)
					filterThem(xs,ns)(f)
				}
				else {
					filterThem(xs, appendFold(ns,x))(f)
				}
			}
		}

		if(as == Nil) Nil
		else filterThem(as,List[A]())(f)
	}
}