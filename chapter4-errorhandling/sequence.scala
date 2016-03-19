sealed trait Option[+A]{
	/*
	*	Exercise 4.1 - Implement functions map, flatMap, getOrElse, orElse
	*	and filter on Option.
	*/
	def map[B](f: A => B) : Option[B] = this match{
		case None => None
		case Some(a) => Some(f(a))
	}

	
	def getOrElse[B >: A] (default: => B) : B = this match {
		case None => default
		case Some(a) => a
	}

	def flatMap[B](f: A => Option[B]) : Option[B] = {
		map(f) getOrElse None
	}

	def orElse[B >: A] (ob: => Option[B]) : Option[B] = {
		map(Some(_)) getOrElse ob
	}

	def filter(f: A => Boolean) : Option[A] = {
		flatMap((a: A) => if(f(a)) Some(a) else None)
	}
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

	/*
	*	Exercise 4.3 - Function sequence combines a list of Options into one Option
	*	containing a list of all the Some values in the original list.
	*
	*	Input:
	*	val x = List(Some(1), Some(2), Some(3))
	*	Option.sequence(x)
	*
	*	Expected Output:
	*	Option[List[Int]] = Some(List(1, 2, 3))
	*
	*	Input 2:
	*	val y = List(None, Some(1), Some(2))
	*	Option.sequence(y)
	*
	*	Expected Output:
	*	Option[List[Int]] = None
	*/

	def sequence[A](a: List[Option[A]]) : Option[List[A]] = a match {
		case Nil => Some(Nil)
		case x :: xs => x flatMap(x2 => (sequence(xs) map (x2 :: _))) 
	}
}