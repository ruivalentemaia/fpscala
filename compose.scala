/*
* Exercise 2.5 - implement the high-order function that composes
* two given functions.
*/
def compose[A,B,C](f: B => C, g: A => B): A => C = {
	a:A => f(g(a))
}