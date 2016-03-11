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
	*	Exercise 3.16 - Transforms list of integers by adding 1 to each element.
	*
	*	Input example:
	*		val x = List(1,2,3,4)
	*	Output:
	*		List[Int]: x = Cons(2,Cons(3,Cons(4,Cons(5,Nil))))
	*/
	def addOne(ls: List[Int]) : List[Int] = {
		@annotation.tailrec
		def sumThrough(ls: List[Int], ns: List[Int]) : List[Int] = ls match {
			case Nil => ns
			case Cons(x,Nil) => sumThrough(Nil,appendFold(ns,x+1))
			case Cons(x,xs) => sumThrough(xs, appendFold(ns,x+1))
		}

		if(ls == Nil) Nil
		else sumThrough(ls,List[Int]())
	}
}