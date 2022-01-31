module FsharpPart4 

#nowarn "25"    // let's not bother about incomplete pattern matches

// Crash course in F#, part 4

// *** Record types

(*
Records are like tuples, but components are identified
by field names rather than positionally. 
*)

type person = {name : string; age : int}

let j = {name = "john"; age = 35};;

(*
Notice that the same keyword "type" that is used
in datatype definitions is also used in record type definitions.

Notice the use of : in the record type and = in a record.
*)


// * Field selection

(*
Field selection can be done with dot-notation
or by pattern-matching.
*)

// let n = j.name;;

// let {name = n; age = _} = j;;

// let {age = a} = j;;

// let {name = n} = j;;     // gives an error
                            // since the field "name"
                            // does not uniquely identify
                            // a record type;
                            // we also have person3 and person5

// let {person.name = n} = j;;


(*
The order in which the fields are listed in a record
is irrelevant.
*)

// j = {age = 35; name = "john"};;

// let k = { age = 12; name = "joe"};;


// Record types may be recursive.

type person3 = {name : string;
                 father : person3 option; mother : person3 option}

// let j = {name = "john"; father = None; mother = None};;
// let k = {name = "mary"; father = Some j; mother = None};;


(*
Record fields can be mutable, i.e., destructively updateable.
This is the first moment we see something non-purely functional
(imperative) in F#.
*)

type person5 = {name : string; mutable salary : int}

// let j = {name = "john"; salary = 5000};;


// * Updating a field

// j.salary <- 5500;;

(*
Note that this is an expression, not a definition by let.

Note that this expression is of type unit.
It returns () (no information) (but has a side-effect). 
Such expressions are called commands. 
*)

// j;;


// *** References (aka pointers)

(*
A reference points to a memory location holding a value (contents) of
some type.

The basis environment of F# provides a polymorphic reference type as a
record type with a mutable field contents.

type 'a ref = {mutable contents : 'a}     // reference type

// ref : 'a -> 'a ref
let ref x = {contents = x}                // define a new reference,
                                          // initialize contents to x

// ! : 'a ref -> 'a
let ! p = p.contents                      // dereference ref p

// (:=) : 'a ref -> 'a -> unit
let (:=) p y = p.contents <- y            // update ref p to y

! without an argument is not valid F# syntax, but you should think of
the ! operator as defined like this.
*)


// let p = ref 42;;
// let q = ref 1011;;
// !p;;
// !q;;
// let q = p;;          // define q as p
                        // now q and p point to the same location
// !p;;
// !q;;
// q := 17;;
// !p;;


// * Sequencing

(*
The F# language provides operations for sequencing and for
iteration of commands.

// (;) : unit -> 'a -> 'a

let c ; e =
   let _ = c
   e

(;) without arguments is not valid F# syntax, but you should think
of (;) as defined like this.
*)

// q := !q + 1; !q;;

// swap : int ref -> int ref -> unit

let swap p q =
   let r = ref 0                         // r : int ref
   r := !p; p := !q; q := !r  

(*
// swap : 'a ref -> 'a ref -> unit

let swap p q =
   let r = p                             // r : 'a ref
   p := !q; q := !r

let swap p q =
   let r = !p                            // r : 'a
   p := !q; q := r
*)

// let p = ref 15;;
// let q = ref 32;;
// swap p q;;
// !p;;
// !q;;


// * Looping 

(*
// while-do : bool -> unit -> unit

let while b do c =
   if b
   then c ; while b do c
   else ()

Again this is definition is not valid F# syntax, but you should think
of while-do as defined like this.
*)

let fact n =
   let p = ref n
   let q = ref 1
   (while !p > 0 do q := !q * !p; p := !p - 1); !q  

// fact 10;;

(*
This is how you program imperatively in F#.
This is not how you should code the factorial function in F#.
*)

// * A counter with a private state

// tick0 : unit -> int
// reset0 : unit -> unit

let (tick0, reset0) =
    let counter0 = ref 0
    let tick0 () = counter0 := !counter0 + 1; !counter0
    let reset0 () = counter0 := 0
    (tick0, reset0)

// Note that only tick0 and reset0 have access to counter0. 

// tick0 ();;
// tick0 ();;
// tick0 ();;
// reset0 ();;
// tick0 ();;
    

// * A function delivering counters

type Counter = {tick : unit -> int; reset : unit -> unit}

// newCounter : unit -> Counter

let newCounter () =
    let counter = ref 0
    let tick () = counter := !counter + 1; !counter
    let reset () = counter := 0
    { tick = tick; reset = reset }
    
// let c1 = newCounter ();;
// c1.tick ();;
// let c2 = newCounter ();;
// c2.tick ();;
// c2.tick ();;
// c2.tick ();;
// c1.tick ();;  


// * (Possibly) circular lists

(*
The following is a mutually recursive type definition
of circular lists and cells.

The first type definition is a type synonym definition.
The second is a datatype definition.
*)

type 'a clist = ('a cell) ref
and  'a cell  = | Nil | Cons of 'a * 'a clist 

let cnil ()      = ref Nil
let ccons (x, p) = ref (Cons (x, p))


(*
Here's an integer list with a cycle.
*)

let myList =
    let p = ref Nil    // it does not matter what we initialize p to,
                       // we just need an (int cell) ref
    p := Cons (3, ccons (8, ccons (15, p))); ccons (4, p)

// myList;;
                       // [4;3;8;15;3;8;15;3;8;15;..]


// * head and tail for circular lists

// chead :: 'a clist -> 'a
// ctail :: 'a clist -> 'a clist

let chead p = let (Cons (x, _)) = !p in x

(*
let chead p = 
    match !p with
    | Cons (x, _) -> x
*)

let ctail p = let (Cons (_, p)) = !p in p

(*
The patterns in these functions are not exhaustive,
so these functions can give a pattern-match failure error.
*)


// * Safe head and tail

let tryChead p = 
    match !p with 
    | Nil          -> None
    | Cons (x, _)  -> Some x

let tryCtail p = 
    match !p with
    | Nil          -> None
    | Cons (_, p)  -> Some p


// * Converting a list into a circular list

// list2clist :: 'a list -> 'a clist

let rec list2clist xs =
    match xs with
    | []      -> cnil ()
    | x :: xs -> ccons (x, list2clist xs)


// * Extracting n first elements of a circular list

// ctale :: int -> 'a clist -> 'a list

let rec ctake n p =
    match n, !p with
    | 0, _            -> []
    | _, Nil          -> []
    | n, Cons (x, p) -> x :: ctake (n - 1) p

// ctake 8 myList;;


// * Concatenation two circular lists

// cappend0 : 'a clist -> 'a clist -> 'a clist

let rec cappend0 p q =
    match !p with
    | Nil         -> q
    | Cons (x, p) -> ccons (x, cappend0 p q)

(*
Notice that cappend0 not only traverses the first list
but also copies it.

If the first list contains a cycle, it
produces an unwinded copy, so recurses infinitely. 
*)

// let p = list2clist [1;2];;
// let q = list2clist [9;10];;
// cappend0 p q;;
// p;;


// * A better "in-place concatenation" of two circular lists

// append : 'a clist -> 'a clist -> unit

let rec append p q =
    match !p with
    | Nil          -> p := !q
    | Cons (_, p)  -> append p q

(*
The null pointer at the end of the first list is replaced
with a pointer to the first cell of the second list.
The first list has thereby been mutated to the concatenation
of the two lists. 
The function returns () and has a side-effect, i.e. is
a command. 

append still traverses the first list (we do not have
direct access to the end of the first list),
but it does not copy it.

If the first list contains a cycle, append recurses infinitely.
*)

// append p q;;
// p;;


// *** Mutable names

(* 
Similarly to fields of a record, it is also possible 
to declare a name mutable. 

Updating the value of such a name is done with a <- command
similarly to field updating.

Compare:
*)

// let i = 5;;
// let i = i + 1;;           // two immutable names i,
// i;;                       // definition of the inner one
                             // shadows out the outer one
// let mutable i = 5;;
// i <- i + 1;;              // one mutable name i
// i;;                       // updating by a command

(*
If you use mutable names, you cannot see what is mutable 
by looking at types. 

In contrast, a type like t ref is a clear indication 
that values in it are references, so have mutable contents. 
*)



