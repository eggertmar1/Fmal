module FsharpPart2

// FMAL Spring 2021, code for Lecture 2, F# crash course, part 2

(*
You may want to load this file at once by entering these
lines in fsharpi:

#load "FsharpPart2.fs";;
open FsharpPart2;;
*)


// *** Lists

(*
Lists are a polymorphic datatype (recursive discriminated union)
predefined in F#.

A list is a sequence of elements of the same type of any finite
length.

The type of lists over a type t is  t list .

A list is made by iterating the data constructor (tag) (::)
(called "cons") on the data constructor (tag) [] (the empty list).
*)

let xs1 = [7; 9; 13]

// xs1 = 7 :: 9 :: 13 :: [];;

(*
Functions on lists are defined by cases by pattern-matching
against expressions made of data constructors (tags) [] and (::).

sum : int list -> int
prod : int list -> int
*)

let rec sum xs = 
    match xs with 
    | []    -> 0
    | x::xs -> x + sum xs

// sum xs1;;

(*
sum [7;9;13]
   ---> 7 + sum [9;13]
   ---> 7 + (9 + sum [13])
   ---> 7 + (9 + (13 + sum []))
   ---> 7 + (9 + (13 + 0))
   ---> 7 + (9 + 13)
   ---> 7 + 22
   ---> 29
*)

let rec prod xs = 
    match xs with 
    | []    -> 1
    | x::xs -> x * prod xs

// prod xs1;;

(*
Patterns can be nested. Also, the "as" construction allows you to
match a value against both a pattern and a variable at once.

diffs : int list -> int list 
*)

let rec diffs xs =
    match xs with
    | []  -> []
    | [x] -> []         // could also write x::[]
    | x::(y::_ as xs) -> (y-x) :: diffs xs     // x::y::_

// diffs xs1;;



// *** Polymorphic functions

(*
A function is (universally) polymorphic if it works on any type.
*)

// * Length

(*
The function length is defined in the List module.

length : 'a list -> int
*)

let rec length xs = 
    match xs with
    | []    -> 0
    | _::xs -> 1 + length xs

// length [7; 9; 13];;

(*
length [7;9;13]
   ---> 1 + length [9;13]
   ---> 1 + (1 + length [13])
   ---> 1 + (1 + (1 + length []))
   ---> 1 + (1 + (1 + 0))
   ---> 1 + (1 + 1)
   ---> 1 + 2
   ---> 3
*)

// length ["Oslo"; "Aarhus"; "Gothenburg"; "Copenhagen"];;


// * List head and tail

(*
These functions are defined in the List module,
i.e., available to you as List.head and List.tail.

head : 'a list -> 'a
tail : 'a list -> 'a list
*)

let head xs =
    match xs with
    | x::_  -> x

let tail xs =
    match xs with
    | _::xs -> xs

(*
The patterns here are incomplete. This is fine by the F#
type-checker, but means that
head and tail lead to a pattern-match failure error
at runtime.
*)

// * List concatenation (also called append)

(*
The concatenation function (@) is provided by the basis environment
(rather than only in the List module).
It is defined like this.

(@) : 'a list -> 'a list -> 'a list
*)

(*
let rec (@) xs ys =
    match xs with
    | []    -> ys
    | x::xs -> x :: (@) xs ys
*)

// [7; 9; 13] @ [47; 11];;


// * Getting the nth element

(*
item is defined in the List module.

item : 'a list -> int -> 'a 
*)

let rec item i xs =
    match i, xs with
    | 0, x::_   -> x
    | i, _::xs  -> item (i-1) xs

(*
Notice pattern-matching against a pair.

Notice that there is no pattern i, [] .
*)


// *** Higher-order functions

(*
A higher-order function is one that takes a function as an argument.

Here are some such functions capturing some very useful abstractions.
*)

// * map

(*
With map you can apply one and the same operation to every
element of a list.

map is defined in the List module.

map : ('a -> 'b) -> 'a list -> 'b list 
*)

let rec map f xs = 
    match xs with 
    | []    -> []
    | x::xs -> f x :: map f xs

let mul2 x = 2.0 * x

// map mul2 [4.0; 5.0; 89.0];;

let isLarge x = 10.0 < x

// map isLarge [4.0; 5.0; 89.0];;

(*
length could be defined from sum using map.

length : 'a list -> int
*)

(*
let length xs = sum (map (fun _ -> 1) xs)
*)

(*
Function composition can written with << or >>,
depending on which way around you want to compose functions. 
*)

(*
let length xs = (sum << map (fun _ -> 1)) xs
*)

(*
You can make your code even more cryptic using constant,
which is a function that ignores its second argument
and returns the first.
*)

(*
let constant c _ = c
let length xs = sum (map (constant 1) xs)
*)


// * fold   (called foldl in some other functional languages)

(*
fold is a function traversing a list and accumulating a value
by applying an operation to the accumulator and the next element..

fold is defined in the List module.

fold : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b

It corresponds to looping over a list;
the accumulator corresponds to the loop control variable of imp prog.
*)

let rec fold f acc xs = 
    match xs with
    | []    -> acc
    | x::xs -> fold f (f acc x) xs

(*
Here are length, sum, prod written using fold.
*)

(*
let length xs =
    let rec length' l xs =
         match xs with
	 | []  -> l
	 | _::xs -> length' (l + 1) xs
    length' 0 xs
*)

(*
length [7;9;13]
   ---> length' 0 [7;9;13]
   ---> length' (0 + 1) [9;13]
   ---> length' 1 [9;13]
   ---> length' (1 + 1) [13]
   ---> length' 2 [13]
   ---> length' (2 + 1) []
   ---> length' 3 []
   ---> 3
*)

(*
let length xs = fold (fun l _ -> l + 1) 0 xs

let sum xs = fold (fun s x -> s + x) 0 xs
let sum xs = fold (+) 0 xs

let prod xs = fold (fun p x -> p * x) 1 xs
let prod xs = fold (*) 1 xs
*)


// * foldBack   (called foldr in some other functional languages)

(*
foldBack is a function recursing on a list and and applying
an operation to the head and the result from the recursive call.

foldBack is defined in the List module.

foldBack : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
*)

let rec foldBack f xs c =
    match xs with 
    | []    -> c
    | x::xs -> f x (foldBack f xs c)

(*
Here is (@) defined using foldBack.
*)

(*
let (@) xs ys = foldBack (fun x zs -> x :: zs) xs ys
*)

(*
Concatenation can also be defined with fold,
but the resulting function is inefficient - quadratic
rather than linear - why?
*)

let rec snoc xs y =
    match xs with
    | [] -> [y]
    | x::xs -> x :: snoc xs y

(*  
let (@) xs ys = fold (fun acc -> fun y -> snoc acc y) xs ys
*)

(*
You can also write it shorter:

let (@) xs ys = fold (fun acc y -> snoc acc y) xs ys
*)

// * filter

(*
filter allows you to keep only values satisfying a given predicate.
It is defined in the List module.

filter : ('a -> bool) -> 'a list -> 'a list
*)

let rec filter p xs =
   match xs with 
   | []    -> []
   | x::xs -> if p x then x :: filter p xs else filter p xs

(* Here is filter defined using foldBack. *)

(*
let filter p xs = foldBack (fun x zs -> if p x then x :: zs else zs) xs []
*)

(* Defining filter with fold is again not such a great idea. *)

(*
let filter p xs = fold (fun acc x -> if p x then snoc acc x else acc) [] xs
*)

