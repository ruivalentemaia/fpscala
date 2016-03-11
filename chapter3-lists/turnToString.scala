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
	*	Exercise 3.17 - Turns each value in a list of doubles into a string.
	*/
	def turnToString(ls:List[Double]) : List[String] = {
		@annotation.tailrec
		def throughToString(ls: List[Double], ns: List[String]) : List[String] = ls match {
			case Nil => ns
			case Cons(x,Nil) => throughToString(Nil, appendFold(ns,x.toString))
			case Cons(x,xs) => throughToString(xs, appendFold(ns,x.toString))
		}

		if(ls == Nil) Nil
		else throughToString(ls,List[String]())
	}
}