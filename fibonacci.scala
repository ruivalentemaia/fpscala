/* 
* Exercise 2.1 - Gets the nth fibonacci number.
*/
def fib (n:Int) : Int = {
	@annotation.tailrec
	def go(n:Int, prev:Int, acc:Int) : Int = {
		if (n <= 1) acc
		else go(n-1, acc, prev+acc)
	}
	go(n, 0, 1)
}