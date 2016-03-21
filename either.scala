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
