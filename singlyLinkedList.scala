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

	/*
	* Exercise 3.2 - Removes the first element of a List.
	*/
	def tail[A](l: List[A]) : List[A] = l match {
		case Nil => l
		case Cons(x,xs) => xs
	}

	/*
	* Exercise 3.3 - Replace the first element of a List with a given
	* different value.
	*/
	def setHead[A](l:List[A], elem: A) : List[A] = l match {
		case Nil => l
		case Cons(Nil, xs) => Cons(elem, xs)
		case Cons(x,xs) => Cons(elem,xs)
	}

	/*
	* Exercise 3.4 - Remove first n elements from
	* a list, by generalizing tail to this function.
	*
	* Test Case:
	*	val x = List(1,2,3,4,5)
	*	List.drop(x,2)
	*
	* Expected Output:
	*	List[Int] = Cons(3,Cons(4,Cons(5,Nil)))
	*/
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
		def appendElem(nl: List[A], elem: A): List[A] = nl match {
			case Nil => Cons(elem,Nil)
			case Cons(h,t) => Cons(h,appendElem(t,elem))
		}
		def rmlast(nl: List[A], sl: List[A]) : List[A] = nl match{
			case Nil => Nil
			case Cons(h,Nil) => sl
			case Cons(h,t) => rmlast(t,appendElem(sl,h))
		}
		if(l == Nil) l
		else rmlast(l,List[A]())
	}

}
