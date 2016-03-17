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