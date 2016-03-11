/*
* Exercise 2.2 - Checks if an Array of type A is ordered according to a given
* comparison function "ordered".
*/
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
	@annotation.tailrec
	def loop(n:Int) : Boolean = 
		if(n >= as.length - 1) true
		else if (ordered(as(n),as(n+1))) loop(n+1)
		else false
	loop(0)
}