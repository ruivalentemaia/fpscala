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
	*	Exercise 4.2 - Function variance, implemented in terms of flatMap
	*	and the computation should be aborted as soon as the first failure
	* 	is encountered.
	*	
	*	Input:
	*	:paste option.scala
	*	val x = Seq(1.5,6.7,7.6,1.0,-0.3)
	*	Option.variance(x)
	*
	*	Expected Output:
	*	Option[Double] = Some(10.308)
	*/

	def mean(xs: Seq[Double]) : Option[Double] = {
		if(xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def variance(xs: Seq[Double]) : Option[Double] = {
		mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))
	}

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

	/*
	*	Exercise 4.4 - Function sequence combines a list of Options int one Option
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

	/*
	*	Exercise 4.5 - Function traverse, which sequences the results
	*	of a map, converting each element of a List[A] to a Option[List[A]].
	*
	*	Input:
	*	val x = List(1,2,3,4)
	*	Option.traverse(x)(x => Some(x.toString))
	*
	*	Expected Output:
	*	Option[List[String]] = Some(List(1,2,3,4))
	*/
	def traverse[A,B](a: List[A]) (f: A => Option[B]) : Option[List[B]] = {
		def traverseOpt(a: List[A]) : Option[List[B]] = a match {
			case Nil => Some(Nil)
			case x::xs => traverse(xs) (x => f(x))
		}

		traverseOpt(a)
	}
}
