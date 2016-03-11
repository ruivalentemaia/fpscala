/*
* Exercise 2.3 - Converts a function f of 2 arguments into a 
* function of one argument that partially applies f.
*/
def curry[A,B,C] (f: (A,B) => C) : A => (B => C) =
	(a: A) => ((b: B) => f(a,b))

/*
* Exercise 2.4 - Reverses the transformation of curry.
*/
def uncurry[A,B,C] (f: A => B => C) : (A,B) => C =
	(a:A,b:B) => f(a:A)(b:B)