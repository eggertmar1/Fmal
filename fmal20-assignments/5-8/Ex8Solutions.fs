// T-501-FMAL Programming languages, Practice class 8
// Spring 2021
// Solutions


module Ex8Solutions

// Problem 1

// (i)

(*
let f x = false in f

f : 'a -> 'b
x : 'a 
f x : 'b

'b = bool   

f : 'a -> bool
*)


// (ii)

(*
let f x = (let g y = y in g false) in f

f : 'a -> 'b 
x : 'a
f x : 'b

  g : 'c -> 'd
  y : 'c
  g y : 'd

  'd = 'c

  g : 'c -> 'c

g : forall 'c. 'c -> c'    // type of g generalizes

g : 'e -> 'e               // specialization for g false

'e = bool

g : bool -> bool

'b = bool

f : 'a -> bool
*)


// (iii)

(*
let f x = (let g y = y in g (g 3 < 4)) in f

f : 'a -> 'b 
x : 'a
f x : 'b

  g : 'c -> 'd
  y : 'c
  g y : 'd

  'd = 'c

  g : 'c -> 'c

g : forall 'c. 'c -> c'    // general type of g

g : 'e -> 'e               // type of g in g 3
g : 'f -> 'f               // type of g in g (g 3 < 4)

'e = int

g : int -> int             // type of g in g 3

'f = bool

g : bool -> bool           // type of g in g (g 3 < 4)

'b = bool

f : 'a -> bool
*)


// (iv)

(*
let f x = (let g y = if true then y else x in g false) in f

f : 'a -> 'b 
x : 'a
f x : 'b

  g : 'c -> 'd
  y : 'c
  g y : 'd

  'd = 'a
  'd = 'c

  g : 'a -> 'a   // We could think g : forall 'a. 'a -> 'a.
                 // But that's wrong.

g : 'a -> 'a               // nothing to generalize and then specialize

'a = bool

g : bool -> bool

'b = bool

f : bool -> bool
*)


// (v)

(*
let f x = (let g y = if true then y else x < 3 in g false) in f

f : 'a -> 'b 
x : 'a
f x : 'b

  g : 'c -> 'd
  y : 'c
  g y : 'd

  'a = int
  'c = bool
  'd = 'c

  g : bool -> bool

g : bool -> bool             // nothing to generalize and then specialize

'b = bool

f : int -> bool
*)


// (vi)

(*
let f x = x + 3 in let g h = h (f 8) in g

f : 'a -> 'b
x : 'a
f x : 'b

'a = int
'b = int

f : int -> int

g : 'c -> 'd
h : 'c
g h : 'd

'c = 'e -> 'f

h : 'e -> 'f
g : ('e -> 'f) -> 'd

'e = int

h : int -> 'f
g : (int -> 'd) -> 'd

'd = 'f

h : int -> 'd
g : (int -> 'd) -> 'd
*)


// (vii)

(*
let rec f x = f x + 1 in f

f : 'a -> 'b
x : 'a
f x : 'b

'b = int 

f : 'a -> int
*)


// (viii)

(*
let rec f x = f (x + 1) in f

f : 'a -> 'b
x : 'a
f x : 'b

'a = int    

f : int -> 'b
*)



// Problem 2

// bool -> bool
let b2b b = not b

// int -> bool
let i2b n = n > 17

// (int -> bool) -> int
let rec minim p = if p 0 then 0 else 1 + minim (fun n -> p (1 + n))

// 'a -> 'b -> 'b
let second x y = y 

// ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
let comp f g x = g (f x) 

// ('a -> 'b) -> ('a -> 'b -> 'c) -> 'a -> 'c
let comp' f g x = g x (f x) 
   
// ('a -> 'a -> 'b) -> 'a -> 'b
let diag f x = f x x

// ('a -> 'e -> 'd) -> ('b -> 'c -> 'e) -> 'a -> 'b -> 'c -> 'd
let func f g x y z = f x (g y z) 
