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
	*	Exercise 4.3 - Generic function map2 that combines two Option values
	*	using a binary function.
	*
	*	Input:
	*	def Try[A](a: => A) : Option[A] = {
	*		try Some(a)
	*		catch { case e: Exception => None }
	*	}
	*
	*	def convert(a:Int) : Option[Int] = {
	*		return Try{ a.toInt }
	*	}
	*
	*	def mult(a:Int, b:Int) = a * b
	*
	*	val a = "10"
	*	val b = "10"
	*	Option.map2(convert(a),convert(b))(mult)
	*
	*	Expected Output: Option[Int] = Some(100)
	*
	*/
	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C) : Option[C] = (a,b) match {
		case (Some(a), Some(b)) => Some(f(a,b))
		case _ => None
	}
}
