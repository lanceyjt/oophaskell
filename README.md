Object Oriented Programming in Haskell
======================================

Overview
--------
The project is based on the paper Dimensions of Object-Based Language Design (Wegner, 1987). It includes the implementation of several concepts of object-based languages, including objects, classes, inheritance, data abstraction and concurrency. Interested in the capability of implementing the concepts above with functional programming language, this project will use Haskell as the target language, with some unit testing and feature testing to evaluate the successful implementation of the concepts above.

Goal
----
1.	Define “Class” concept in object-based languages in Haskell. This could be handled by the nature of algebraic data type in Haskell.
2.	Create “Objects” from different classes.
3.	Implement operations/functions in different classes. Here we will implement functions to access x index of points, center of circles, vertex of rectangles, and functions to set new values on x index.
4.	Implement “Data abstraction” concept, accessing state of an object through operations.
5.	Implement inheritance between classes. Here Point class is inherited by Circle and Rectangle class, the function getX, setX and modifyX can be called by objects in Point, Circle and Rectangle class. Rectangle class is inherited by NewRectangle Class, the function getRectangle, isValidRectangle, and isSquare_ can be called in by objects in Rectangle and NewRectangle class.
6.	Create multiple objects concurrently and execute operations concurrently.

Note
----
All the algebraic data types and functions will be in path:

  src/Lib.hs

Tests will be in path:

  Test/Spec.hs

Tests
-----

1.	Test the creation of objects. 
Test whether functions constructPoint, constructCircle, constructRectangle are able to create objects in Point, Circle and Rectangle Classes separately. 

2.	Test the function of accessing X index (a state of Point object).
Test whether getX can access:
 - X index of an object in Point Class
 - X index of the center of an object in Circle Class
 - X index of the upper-left/lower-right vertex of an object in Rectangle Class

3.	Test the operation of setting new values on X index.
Test whether SetX can set new values on:
 - X index of an object in Point Class
 -	X index of the center of an object in Circle Class
 -	X index of the upper-left/lower-right vertex of an object in Rectangle Class

4.	Test the operation of applying functions on X index.
Test whether modifyX can apply functions on:
 - X index of an object in Point Class
 - X index of the center of an object in Circle Class
 - X index of the upper-left/lower-right vertex of an object in Rectangle Class

5.	Test whether functions in NewRectangle Class inherited from Rectangle Class work as expected.

Running Code
------------
Run the command to initialize stack once:

``` {.sh}
$ stack init
```

To run the code, start GHCi with `stack ghci`:

``` {.sh}
$ stack ghci
```

To run the test-suite, run command 

``` {.sh}
$ stack test
```


