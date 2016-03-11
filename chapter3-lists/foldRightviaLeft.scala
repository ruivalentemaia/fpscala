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

	def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B) : B = {
		@annotation.tailrec
		def go(a: List[A], z: B, ac: B) (f: (B,A) => B) : B = 
			a match {
				case Nil => z
				case Cons(Nil,xs) => go(xs,z,z)(f)
				case Cons(x,Nil) => f(ac,x)
				case Cons(x,xs) => go(xs,z,f(ac,x))(f)
			}
		if (as == Nil) z
		else go(as,z,z)(f)
	}

	/*
	* 	Exercise 3.13 - Write foldRight via foldLeft.
	*/
	def foldRightviaLeft[A,B](as: List[A], z: B)(f: (A,B) => B) : B = {
		as match {
			case Nil => z
			case Cons(x,Nil) => foldLeft(as,f(x,z)) ((x,y) => f(y,x))
			case Cons(x,xs) => foldLeft(xs,f(x,z)) ((x,y) => f(y,x))		
		}
	}

	def sumFoldRightLeft(ns: List[Int]) = 
		foldRightviaLeft(ns,0)((x,y) => x + y)

	def multFoldRightLeft(ns: List[Double] ) = 
		foldRightviaLeft(ns,1.0)((x,y) => x * y)

	def lengthRightLeft[A](ns: List[A]) : Int =
		foldRightviaLeft(ns,0)((x,y) => 1 + y)
}