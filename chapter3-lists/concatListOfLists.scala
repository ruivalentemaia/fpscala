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
	*	Exercise 3.15 - Concatenates list of lists into single list
	*/
	def concList[A](ls: List[List[A]]) : List[A] = {
		@annotation.tailrec
		def getHead(ls: List[A]) : A = ls match {
			case Nil => getHead(ls)
			case Cons(x,Nil) => x
			case Cons(x,xs) => x 
		}

		@annotation.tailrec
		def transform(ls: List[A], ns: List[A]) : List[A] = ls match {
			case Nil => ns
			case Cons(x,Nil) => appendFold(ns,x)
			case Cons(x,xs) => transform(xs,appendFold(ns,x))
		}

		@annotation.tailrec
		def convert(ls: List[List[A]], ns: List[A]) : List[A] = ls match {
			case Nil => ns
			case Cons(Nil,xs) => convert(xs,ns)
			case Cons(x,Nil) => transform(x,ns)
			case Cons(x,xs) => convert(xs, transform(x,ns))
		}

		if(ls == Nil) Nil
		else convert(ls,List[A]())
	}
}