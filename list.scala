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

	/*
	* Exercise 3.7 - Can product, implemented using foldRight,
	* immediately halt the recursion and return 0.0 if it encounters
	* a 0.0 ?
	*	Answer: according to my implementation, yes.
	*
	* Test Case:
	*	1)
	*	val x = List(1.0,2.0,3.0,4.0,0.0,5.0,6.0)
	*	List.productHalt(x)
	*
	* 	Expected Output: 0.0
	*
	*	2)
	*	val x = List(1.0,2.0,5.0)
	*	List.productHalt(x)
	*
	*	Expected Output: 10.0	
	*/
	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
		as match {
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs, z)(f))
		}

	def sumFold(ns: List[Int]) =
		foldRight(ns, 0)((x,y) => x + y)


	def productFold(ns:List[Double]) =
		foldRight(ns, 1.0)((x,y) => x*y)

	def productHalt(ns:List[Double]) =
		ns match {
			case Nil => foldRight(Nil,1.0)((x,y) => x)
			case Cons(h,t) => {
				if(h == 0.0) foldRight(Nil, 0.0)((x,y) => x)
				else foldRight(ns,1.0)((x,y) => x*y)
			}
		}

	/*
	* Exercise 3.8 - What happens when you do: 
	* List.foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_))
	*
	* Answer:
	*	List[Int] = Cons(1,Cons(2,Cons(3,Nil)))
	*/

	/*
	* Exercise 3.9 - Function 'length' should compute the length
	* of a list using foldRight.
	*
	* Test Case:
	* 	1)
	*	val x = List(1,2,3,4,5)
	*	List.length(x)
	*foldRight
	*	Expected Output: 5
	*
	*	2)
	*	val x = List()
	*	List.length(x)
	*	
	*	Expected Output: 0
	*/
	def length[A](as: List[A]) : Int =
		foldRight(as, 0)((x,y) => 1 + y)

	/*
	*	Exercise 3.10 - Function 'foldLeft', similar to 'foldRight',
	*	but tail-recursive.
	*/
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
	*	Exercise 3.11 - Functions 'sumLeft', 'productLeft' and
	* 	'lengthLeft' to compute the length of a list using
	* 	foldLeft.
	*/
	def sumLeft(ns: List[Int]) =
		foldLeft(ns,0)((x,y) => x + y)

	def productLeft(ns: List[Double]) = 
		foldLeft(ns, 1.0) ((x,y) => x * y)

	def lengthLeft[A](as: List[A]) : Int = 
		foldLeft(as,0)((x,y) => 1 + x)

	/*
	*	Exercise 3.12 - Reverses a list.
	*/
	def reverse[A](ls: List[A]) : List[A] = {
		@annotation.tailrec
		def go(ls: List[A], ns: List[A]) : List[A] = 
			ls match {
				case Nil => Nil
				case Cons(x,Nil) => Cons(x,ns)
				case Cons(x,xs) => go(xs, Cons(x,ns))
			}
		if(ls == Nil) Nil
		else go(ls, List[A]())
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

	/*
	*	Exercise 3.14 - Append.
	*/
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

	/*
	*	Exercise 3.18 - Function map, that generalizes modifying each element
	*					in a list while maintaining the structure of the list.
	*/
	def map[A,B](as: List[A]) (f: A => B) : List[B] = {
		@annotation.tailrec
		def mapThem(as:List[A], ns:List[B]) : List[B] = as match {
			case Nil => ns
			case Cons(x,Nil) => mapThem(Nil, appendFold(ns, f(x)))
			case Cons(x,xs) => mapThem(xs, appendFold(ns, f(x)))
		}

		if(as == Nil) Nil
		else mapThem(as,List[B]())
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

	/*
	*	Exercise 3.22 - Function "zip", which accepts two lists of integers
	* 					and constructs a new list by adding corresponding
	*					elements.
	*
	*	Input: List.zip(List(1,2,3), List(4,5,6))
	*	Expected Output: List[Int] = List(5,7,9)
	*/

	def zip(ls: List[Int], ns: List[Int]) : List[Int] = (ls,ns) match {
		case (_,Nil) => ls
		case (Nil,_) => ns
		case (Cons(x,xs),Cons(h,t)) => Cons(x + h, zip(xs,t))
	}

	/*
	*	Exercise 3.23 - Function "zipWith", that performs what Exercise 3.22
	*					zip function does, but it's not specific to integers
	*					or addition.
	*
	*	Input: def subLists(ls: List[Double], ns: List[Double]) : List[Double] =
	*				List.zipWith(ls,ns)((x,y) => x - y)
	*			subLists(List(5.0,5.0,6.0),List(4.0,4.0,5.0))
	*	Expected Output: List[Double] = List(1.0,1.0,1.0)
	*/
	def zipWith[A,B,C](ls: List[A], ns: List[B]) (f: (A,B) => C) : List[C] = (ls,ns) match {
		case (_,Nil) => Nil
		case (Nil,_) => Nil
		case (Cons(x,xs),Cons(h,t)) => Cons(f(x,h), zipWith(xs,t)(f))
	}

	/*
	*	Exercise 3.24 - Function "hasSubsequence", which checks whether a List
	*					contains another List as a subsequence.
	*
	*	Input 1: List.hasSubsequence(List(1,2,3,4),List(1,2))
	*	Expected Output 1: True
	*
	*	Input 2: List.hasSubsequence(List(1,2,3,4), List(1,5))
	* 	Expected Output 2: False
	*/
	def hasSubsequence[A](sup: List[A], sub: List[A]) : Boolean = (sup,sub) match {
		case (Nil,_) => false
		case (_,Nil) => false
		case (Cons(x,xs), Cons(h,Nil)) => {
			if(x == h) true
			else hasSubsequence(xs,sub)
		}
		case (Cons(x,xs), Cons(h,t)) => {
			if(x == h) hasSubsequence(xs,t)
			else hasSubsequence(sup, t)
		}
	}


}
