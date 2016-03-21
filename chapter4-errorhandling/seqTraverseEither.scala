sealed trait Either[+E, +A] {
	
	/*
	*	Exercise 4.6 - Implement versions of map, flatMap, orElse and map2
	*	for the Either data type.
	*/
	def map[B](f: A => B) : Either[E,B] = this match {
		case Left(e) => Left(e)
		case Right(a) => Right(f(a))
	}

	def flatMap[EE >: E,B](f: A => Either[EE,B]) : Either[EE,B] = this match {
		case Left(e) => Left(e)
		case Right(a) => f(a)
	}

	def orElse[EE >: E,B](b: => Either[EE,B]) : Either[EE,B] = this match {
		case Left(e) => Left(e)
		case Right(a) => b
	}

	def map2[EE >: E,B,C](b: Either[EE,B]) (f: (A,B) => C) : Either[EE,C] = 
		this.flatMap(a => b.map(b1 => f(a,b1)))

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]

object Either {
	/*
	*	Exercise 4.7 - Implement sequence and traverse for Either. These should
	*	return the first error that is encountered.
	*	
	*	Input:
	*	val x = List(1,2,3,4)
	*	def trav(a: List[Int]) : Either[String,List[Double]] = {
	*		if(a == null) Left("List is null.")
	*		else Either.traverse(a)(a => Right(a.toDouble))
	*	}
	*	
	*	trav(x)
	*
	*	Expected Output:
	*	Either[String, List[Double]] = Right(List(1.0,2.0,3.0,4.0))
	*
	*	Input 2:
	*	val x = List()
	*	trav(x)
	*
	*	Expected Output:
	*	Either[String, List[Double]] = Left(List is null.)
	*	
	*/
	def sequence[E,A](ee: List[Either[E,A]]) : Either[E,List[A]] = ee match {
		case Nil => Right(Nil)
		case x :: xs => x flatMap(x2 => sequence(xs) map (x2 :: _))
	}

	def traverse[E,A,B](as: List[A])(f: A => Either[E,B]) : Either[E,List[B]] = {
		def put(elem: A, b: List[Either[E,B]]) : List[Either[E,B]] = {
			b ::: List(f(elem))
		}

		def traverseOpt(a: List[A], b: List[Either[E,B]]) : Either[E,List[B]] = a match{
			case Nil => sequence(b)
			case x :: xs => traverseOpt(xs, put(x,b))
		}

		traverseOpt(as,List[Either[E,B]]())
	}
}