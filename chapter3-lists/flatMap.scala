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

	def appendFold[A](ls: List[A], z:A) : List[A] = ls match{
		case Nil => List[A](z)
		case Cons(Nil,xs) => Cons(Nil,appendFold(xs,z))
		case Cons(x,Nil) => List[A](x,z)
		case Cons(x,xs) => Cons(x,appendFold(xs,z))
	}
	
	/*
	*	Exercise 3.20 - Function flatMap, that works like map except that the
	* 					function given will return a list instead of a single
	*					result.
	*
	*	Input Example: flatMap(List(1,2,3)) (i => List(i,i))
	*	Output: List(1,1,2,2,3,3)
	*/
	def flatMap[A,B](as: List[A]) (f: A => List[B]) : List[B] = {

		@annotation.tailrec
		def appendList(bs: List[B], ns: List[B]) : List[B] = bs match{
			case Nil => ns
			case Cons(x,Nil) => appendList(Nil, appendFold(ns,x))
			case Cons(x,xs) => appendList(xs, appendFold(ns,x))
		}

		@annotation.tailrec
		def flatMapThem(as: List[A], ns: List[B]) : List[B] = as match {
			case Nil => ns
			case Cons(x,Nil) => appendList(f(x),ns)
			case Cons(Nil,xs) => flatMapThem(xs,ns)
			case Cons(x,xs) => flatMapThem(xs, appendList(f(x),ns))
		}

		if(as == Nil) Nil
		else flatMapThem(as, List[B]())
	}
}