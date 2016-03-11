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
	* Exercise 3.6 - Function init should return a List consisting
	* of all but the last element of a given List.
	*
	*	Test Case:
	*		val x = List(1,2,3,4,5,6)
	*		List.init(x)
	*
	*	Expected Output:
	*		List[Int] = Cons(1,Cons(2,Cons(3, Cons(4, Cons(5,Nil)))))
	*
	*
	* Special Note:
	* 	After checking the solutions available, I realized I could have
	* 	done this with less lines of code and without using a new list.
	*	It would become a pure function but not tail recursive.
	*/

	def init[A](l: List[A]) : List[A] = {
		def appendElem(nl: List[A], elem: A): List[A] = 
			nl match {
				case Nil => Cons(elem,Nil)
				case Cons(h,t) => Cons(h,appendElem(t,elem))
			}
		
		@annotation.tailrec
		def rmlast(nl: List[A], sl: List[A]) : List[A] = 
			nl match{
				case Nil => Nil
				case Cons(h,Nil) => sl
				case Cons(h,t) => rmlast(t,appendElem(sl,h))
			}
		
		if(l == Nil) l
		else rmlast(l,List[A]())
	}
}