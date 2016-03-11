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

	def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
		as match {
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs, z)(f))
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
}