# Functional Programming in Scala - Solutions to Exercises
Exercises from the book Functional Programming in Scala (Paul Chiusano, Rúnar Bjarnason).

##Exercises

###Chapter 2 - Getting Started

* [Exercise 2.1 - The nth Fibonacci number](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-intro/fibonacci.scala)
* [Exercise 2.2 - Is the Array sorted ?](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-intro/issorted.scala)
* [Exercise 2.3 - Currying](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-intro/curry.scala)
* [Exercise 2.4 - Uncurrying](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-intro/curry.scala)
* [Exercise 2.5 - Compose two functions](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-intro/compose.scala)

###Chapter 3 - Data Structures

####Lists ([All Exercises](https://github.com/ruivalentemaia/fpscala/blob/master/list.scala))

* [Exercise 3.1 - Result of match](https://github.com/ruivalentemaia/fpscala/blob/master/chapter2-lists/singlylinkedlistmatch.scala)
* [Exercise 3.2 - Remove first element of a List](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/tail.scala)
* [Exercise 3.3 - Replace first element of a List](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/setHead.scala)
* [Exercise 3.4 - Remove first n elements of a List](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/drop.scala)
* [Exercise 3.5 - Remove elements of a List while they match a predicate](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/dropWhile.scala)
* [Exercise 3.6 - Returns List of all but the last element](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/init.scala)
* [Exercise 3.7 - Halts recursion if it finds a zer0](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/foldRight.scala)
* [Exercise 3.8 - Direct question](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/length.scala)
* [Exercise 3.9 - Function length using foldRight](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/length.scala)
* [Exercise 3.10 - Function foldLeft, similar to foldRight, but tail-recursive](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/foldLeft.scala)
* [Exercise 3.11 - sumLeft, productLeft and lengthLeft using foldLeft](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/foldLeft.scala)
* [Exercise 3.12 - Reverse a list](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/reverse.scala)
* [Exercise 3.13 - Write foldRight via foldLeft](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/foldRightviaLeft.scala)
* [Exercise 3.14 - Append](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/append.scala)
* [Exercise 3.15 - Transform list of lists into single list](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/concatListOfLists.scala)
* [Exercise 3.16 - Transform by adding 1](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/addOne.scala)
* [Exercise 3.17 - Turns values of list of Double to String](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/turnToString.scala)
* [Exercise 3.18 - Map function](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/map.scala)
* [Exercise 3.19 - Filter function](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/filter.scala)
* [Exercise 3.20 - Works like map but returns a List as result](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/flatMap.scala)
* [Exercise 3.21 - FlatFilter: uses flatMap to implement a filter](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/flatFilter.scala)
* [Exercise 3.22 - Adds two integer lists](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/zip.scala)
* [Exercise 3.23 - Generalized function that performs an operation with two lists of a given type](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/zipWith.scala)
* [Exercise 3.24 - Checks whether a List contains a sublist](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-lists/hasSubsequence.scala)

####Trees ([All Exercises](https://github.com/ruivalentemaia/fpscala/blob/master/tree.scala))

* [Exercise 3.25 - Computes size of a Tree](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-trees/size.scala)
* [Exercise 3.26 - Computes maximum value of a Tree](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-trees/maximum.scala)
* [Exercise 3.27 - Computes maximum length of a Tree](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-trees/depth.scala)
* [Exercise 3.27 - Modifies elements of a Tree given a higher order function](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-trees/map.scala)
* [Exercise 3.28 - Function fold that generalizes over functions size, maximum, depth and map](https://github.com/ruivalentemaia/fpscala/blob/master/chapter3-trees/fold.scala)

###Chapter 4 - Error Handling

####Option ([All Exercises](https://github.com/ruivalentemaia/fpscala/blob/master/option.scala))

* [Exercise 4.1 - Implement basic functions for the custom Option object](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/customOption.scala)
* [Exercise 4.2 - Calculate variance of a Seq in terms of flatMap](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/variance.scala)
* [Exercise 4.3 - Generic function map2 that combines two Option values using a binary function](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/map2.scala)
* [Exercise 4.4 - Function sequence combines a list of Option into one Option containing a list of all Some values in the original list](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/sequence.scala)
* [Exercise 4.5 - Function traverse which sequences results of a map and function sequence via traverse](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/traverse.scala)

####Either ([All Exercises](https://github.com/ruivalentemaia/fpscala/blob/master/either.scala))

* [Exercise 4.6 - Implement basic functions for the custom Either object](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/customEither.scala)
* [Exercise 4.7 - Implement traverse and sequence for the Either object](https://github.com/ruivalentemaia/fpscala/blob/master/chapter4-errorhandling/seqTraverseEither.scala)

One doubt that I had as soon as I've finished the exercises in the book was: when should I use Option or Either ? Will I be able to identify, in the future, depending on the circumstance of what I'm implementing at the moment, which one to use ? This blog post provides a good answer: [Try, Option or Either ?](http://blog.xebia.com/try-option-or-either/)


### Chapter 5 - Strictness and Laziness ([All Exercises](https://github.com/ruivalentemaia/fpscala/blob/master/stream.scala))

* [Exercise 5.1 - Convert Stream to List](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/toList.scala)
* [Exercise 5.2 - Return first n elements of a Stream](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/take.scala)
* [Exercise 5.3 - Return elements of a Stream that match a given predicate](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/takeWhile.scala)
* [Exercise 5.4 - Return true if all Stream elements match predicate](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/forAll.scala)
* [Exercise 5.5 - Implementation of takeWhile via foldRight](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/takeWhileFoldRight.scala)
* [Exercise 5.6 - Implement headOption via foldRight](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/headOptionViaFoldRight.scala)
* [Exercise 5.7 - Map, filter, append and flatMap via foldRight](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/manipulators.scala)
* [Exercise 5.8 - Infinite stream of a given value](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/infinite.scala)
* [Exercise 5.9 - Infinite stream of integers adding one each time](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/infinite.scala)
* [Exercise 5.10 - Infinite stream of Fibonacci numbers](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/infinite.scala)
* [Exercise 5.11 - Generic stream-building function](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/unfold.scala)
* [Exercise 5.12 - fibs, from, constant and ones in terms of unfold](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/unfold.scala)
* [Exercise 5.13 - map, take,takeWhile,zipWith and zipAll in terms of unfold](https://github.com/ruivalentemaia/fpscala/blob/master/chapter5-laziness/zipUnfold.scala)
