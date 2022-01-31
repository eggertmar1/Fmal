module FsharpPart1

// FMAL Spring 2021, code for Lecture 1, F# crash course, part 1 

(*
Based on Peter Sestoft's code for his PLC book.
File Appendix.fs: The meta language F#, a crash course
*)


(*
Copy definitions/expressions from this file into  fsharpi (fsi),
the F# interpreter, one by one.

Each of them has to finish with ;; to signify
you want your definition or expression processed.

To leave the interpreter, use #quit;; .
*)


(*
F# is a functional-first language that is
- strongly statically typed
- strict

and has type inference.

It belongs to the ML family of languages
also including SML, CAML, Caml. 


*Functional-first* means that you program above of all with
expressions, in particular, function expressions.

As a rule, expressions just denote pure values,
they don't produce side-effects like
manipulating memory or performing input-output,

although side-effects are of course supported too.

Computation goes by evaluation,
which means simplification of expressions.

*Strongly statically typed* means that, for your code to be valid,
it must be well-typed, with the type
determined (checked/inferred) ahead of computation.
Any subexpression of your code must obtain a definite type.

Examples of non- strongly typed functional languages 
are languages of the Lisp family.


*Strict (eager)* means that when a function call
(application of a function to an argument) is evaluated,
the function and the argument are evaluated first,
and only then the application.

An example of non-strict (lazy) functional language
is Haskell.


*Having type inference* means that, although
valid code must be well-typed, the programmer
does not need to provide all type annotations
for the language processor to check validity.
The language processor can infer (figure out) types. 
*)


// ** Expressions and names, basic types

// * Arithmetic expressions, the int type 

(*
You can write arithmetic expressions, F# makes
sure it is well-typed, attempting to infer its type.
It then evaluates it for you.

Evaluation (simplification of expressions) is the execution paradigm
of F# or any functional language.

The type of integers is int.
*)

// 3 + 4;;


// * Names and binding

(*
You can give an expression a name - useful if you need it
many times.

If you don't given an expression a name at the fsharpi
prompt, it still gets a name, namely "it".

Names (also called variables) are defined with
definitions of the form

let x = e

Names/variables are immutable, you cannot change a name's
value! So variables don't really vary at all.
*)

let res = 3 + 4;;

// res * 2;;


// * Arithmetic expressions, the float type

(*
Function application is written
as the function followed by the argument.

No parentheses are need around the argument unless it
is a compound expression.
*)

let y = sqrt 2.0;;

(*
The function sqrt takes a float to a float.
You cannot apply it to an int.

There is no implicit conversion whatsoever in F#.
*)

// sqrt 2;;       (* this is ill-typed *)

(*
You need to convert explicitly using cast functions. 
*)

// sqrt (float 2);;

(*
Although there is no implicit conversion,
there is some function name overloading.

(+), for example, is an overloaded function (operator) name.  
*)

// 3 + 4;;
// 3.0 + 4.0;;
// 3 + 4.0;;     (* this is ill-typed *)
// 3.0 + 4;;     (* this is ill-typed *)



// * Using methods from the .NET class library 

(*
Names from a module can be brought to scope by opening the module.

Methods from the .NET class library are available to you
via the module called System. 
*)

open System;;
let z = Math.Sqrt 2.0;;


// * Logical expressions, the bool type

(*
The type bool has as values false and true.

A number of usual logical and comparison operators are
provided.
*)

let large = 10 < res;;


// * Logical operators, conditional expressions

// y > 0.0 && 1.0/y > 7.0;;

// not false ;;

// if 3 < 4 then 117 else 118;;

(*
If-then-else is morally a function of three arguments.  The first
is of type bool. The 2nd and 3rd have to be of the same type.
The return type is the same. 

&&, ||, if-then-else are non-strict
in their 2nd resp 2nd and 3rd arguments!

This is an exception to the general strictness of F#.
*)


(*
The equality test function is = .
NB! It's the same symbol as in let x = e but the meaning is different.
*)

// 5 = 17;;           (* this is a boolean expression *)


// * String-valued expressions, the string type

let title = "Prof.";;
let name = "Lauesen";;
let junk =  "Dear " + title + " " + name + ", You won $$$!";;
// junk.Length;;



// ** Defining functions

(*
You can define a function with a name with a definition of the form

let f x = e 

by giving the function name and then also a parameter name
on the left of "=".

No parentheses are need around the parameter name.

The expression on the right is the "function body".

The scope of the parameter is this right-hand-side expression.

There is no "return" keyword. The function's
return value is the value of the right-hand-side expression.  

The type of a function with argument type t
and return type t' is  t -> t'.

circleArea : float -> float 
*)

let circleArea r = Math.PI * r * r;;

(*
You apply a function by simply supplying an argument
next to the function name.

No parentheses are needed around the argument
if it is not a compound expression. 
*)

// circleArea 10.0;;

let mul2 x = 2.0 * x;;

// mul2 3.5;;

(*
There can also be several parameters.
*)

let makejunk title name = 
    "Dear " + title + " " + name + ", You won $$$!";;
// makejunk "Vice Chancellor" "Tofte";;


// * Anonymous (nameless) functions

(*
A function does not need to be given a name
(especially if you only need it once.)

You just say which name in an expression you want to
consider as a parameter with the fun x -> e
construct. The result is a function expression. 
*)

let mul3 = fun x -> 3.0 * x;;

// mul3 4.0;;

// (fun x -> 5.0 * x) 6.0;;


// let circleArea r = Math.PI * r * r;;

// let circleArea = fun r -> Math.PI * r * r ;; 



// * Recursive function definitions

(*
If a function's definition depends on itself,
then you need to give it a name with let.
Moreover, you need to say it is recursive with rec. 
*)

let rec fac n = if n = 0 then 1 else n * fac (n - 1);;

// fac 7;;


// ** Mutually recursive function definitions

(*
Any name (incl a function name) needs to be defined before it is
used.

If you want to define functions by mutual recursion,
this is not achievable. Then you have to use "and" for
simultaneous definition. 
*)

let rec even n = if n = 0 then true else odd (n-1)
and odd n = if n = 0 then false else even (n-1);;


// ** Type constraints 

(*
F# infers types but it is also possible to give types
for F# to check.
*)

let isLarge (x : float) : bool = 10.0 < x;;

// isLarge 89.0;;


// ** Local let-bindings

(*
Names are scoped with let ... = ... in ... expressions.

An inner definition of the same name shadows out
the outer definition. 
*)

let x = 42;;                    (* outer x is 42     *)
// let x = 9 + 16 in let y = 7 in x * x;;
                                (* inner x is 25     *)
// x;;                          (* the outer x is still 42 *)

(*
Names in F# are immutable.
"Variables" don't really vary at all!

Mutability is achieved with references (pointers). 
*)


// ** Pattern matching

(*
Functions can be defined by cases on the argument by
pattern matching.

You can always pattern-match against a variable.

_ is a special "don't care" wildcard pattern that also
always matches.

At pattern-match, patterns are tried starting
from the first until a matching pattern is found.
Failure of pattern-matching gives a runtime error.
*)

let rec facm n = 
    match n with
    | 0 -> 1    
    | n -> n * facm (n-1);;


(*
For the case that a parameter is pattern-matched directly,
there is a shorter syntax.
*)

(*
let rec facm = fun n -> 
    match n with
    | 0 -> 1
    | n -> n * facm (n - 1);;
*)

let rec faca = 
    function 
    | 0 -> 1
    | n -> n * faca (n - 1);;


// ** Pairs and tuples, product types

(*
Two values of two (generally different) types t1 and t2 can
be combined into a pair of type t1 * t2.

And similarly, n (n > 1) values of n types can t1, ..., tn
can be combined into an n-tuple of type t1 * ... * tn.

*)

let p = (2, 3);;

let w = (2, true, 3.4, "blah");;

(*
Projection from a tuple can be done with pattern-matching.
*)

let (p1, _) = p;;

(*
A two-place function can be defined as a function taking
a pair as an argument, again with pattern-matching.
*)

let add (x, y) = x + y;;

// add (2, 3);;

let noon = (12, 0);;
let talk = (15, 15);;

(*
Nested tuples are possible and very useful.
*)

let earlier ((h1, m1), (h2, m2)) = h1<h2 || (h1=h2 && m1<m2);;


// * The empty tuple () of type unit

(*
The empty tuple is a nullary tuple, a sequence of length 0. 

The type of the empty tuple is   unit. 
It is similar to the type void of C# and Java.
*)

// Console.WriteLine "Hello!";;



// ** Uncurried vs curried functions

(*
There is really nothing like two-place (binary) functions in F#.

You can "fake" a binary function by either 

- a function that takes a pair   (an "uncurried" function)

- a function that takes the first argument 
and returns another function that takes the second one
(a "curried" function).

Functional programmers tend to prefer the curried form
most of the time. 
*)

// addp : int * int -> int

let addp (x, y) = x + y;;

// addp (17, 25);;


// addc : int -> int -> int
// i.e.,  int -> (int -> int),
// but since -> is right-associative we drop the parentheses

let addc x y = x + y;;

// addc 17 25;;

(*
(+) has type int -> int -> int
*)

// (+);;
// (+) 17 25;;

(*
Curried functions are great because they allow
partial application.

However they are biased in that partial application
is smoothly possibly only for the first argument.
*)

let addSeventeen = addc 17;;
// let addSeventeen = (+) 17;;

// addSeventeen 25;;

// addSeventeen 100;;

