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
	*	Exercise 3.21 - Function flatFilter, that uses flatMap to implement filter.
	*
	*	Input Example: flatFilter(List(1,2,3,4,5,6)) (i => List(i%2==0, i%3==0))
	*	Output: List(6)
	*/

	def flatFilter[A](as: List[A]) (f: A => List[Boolean]) : List[A] = {

		@annotation.tailrec
		def checkBooleanList(ls: List[Boolean]) : Boolean = ls match{
			case Nil => return true
			case Cons(x,Nil) => {
				if(x) checkBooleanList(Nil)
				else return false
			}
			case Cons(x,xs) => {
				if(x) checkBooleanList(xs)
				else return false
			}
		}

		def buildList(a : A, ns: List[A]) : List[A] = {
			if(checkBooleanList(f(a))) appendFold(ns,a)
			else ns
		}

		@annotation.tailrec
		def flatFilterThem(as:List[A], ns: List[A]) : List[A] = as match {
			case Nil => ns
			case Cons(x,Nil) => buildList(x,ns)
			case Cons(x,xs) => flatFilterThem(xs, buildList(x,ns))
		}

		if(as == Nil) Nil
		else flatFilterThem(as,List[A]())
	}
}