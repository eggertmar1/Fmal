module FsharpPart3

// Crash course in F#, part 3


// *** Lists once more,
// direct recursion (foldBack) vs tail-recursion

(*
Let's consider one more example of list programming,
the list reversal function.

reverse [17;42;2021]
    ====> [2021;42;17]

This is a case where direct recursion gives
quadratic time whereas with tail-recursion 
one can get linear-time complexity.

That' because with direct recursion one
needs snoc, which is linear-time, whereas with
tail-recursion one can use (::), which is
constant-time. 
*)

let rec snoc xs y =
    match xs with
    | []    -> [y]
    | x::xs -> x :: snoc xs y

(*
let snoc xs y = xs @ [y]
*)

(*
This is list reversal by direction recursion or foldBack.
*)

let rec reverse xs =
    match xs with
    | []    -> []
    | x::xs -> snoc (reverse xs) x

(*
reverse [17;42;2021]
    ---> snoc (reverse [42;2021]) 17
    ---> snoc (snoc (reverse [2021]) 42) 17
    ---> snoc (snoc (snoc (reverse []) 2021) 42) 17
    ---> snoc (snoc (snoc [] 2021) 42) 17
    ---> snoc (snoc [2021] 42) 17
    ---> snoc [2021;42] 17
    ---> [2021;42;17]
*)


// List.foldback : : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

(*
let reverse xs = List.foldBack (fun x -> fun ys -> snoc ys x) xs []
*)

(*
This is list reversal by tail-recursion or fold. 
*)

let reversea xs =
    let rec reverse' acc xs =
        match xs with
        | []    -> acc
        | x::xs -> reverse' (x::acc) xs
    reverse' [] xs

(*
reversea [17;42;2021]
    ----> reverse' [] [17;42;2021]
    ----> reverse' [17] [42;2021]
    ----> reverse' [42;17] [2021]
    ----> reverse' [2021;42;17] []
    ----> [2021;42;17]

*)

// List.fold : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b

(*
let reversea xs = List.fold (fun acc -> fun x -> x :: acc) [] xs
*)



// *** Datatypes (discriminated union types)

(*
Here is a type of persons where a person is either a student
with a name or a teacher with a name and a phone number.
*)

type person =
     | Student of string                // name              
     | Teacher of string * int          // name and phone no

(*
Types that arise in this form of as a union of options
are called datatypes or discriminated union types. 
Student and Teacher are called data constructors or tags.

Student : string -> person
Teacher : string * int -> person
*)

let people = [Student "Niels"; Teacher ("Peter", 5083)]

let getphone p =
    match p with 
    | Teacher (_, phone) -> phone
(*    
    | Student _ -> 0000
*)    

(*
This is an incomplete pattern match!
*)

// getphone (Student "Niels");;
// getphone (Teacher ("Peter", 5083));;



// *** Polymorphic datatypes

// * Node-labelled binary trees

(*
A datatype can be parameterized in a type, it is then
called polymorphic.

Lists are a predefined polymorphic datatype. 

Node-labelled binary trees are another example of a
polymorphic datatype.
*)

type 'a tree =
     | Lf  
     | Br of 'a * 'a tree * 'a tree

(*
Lf : 'a tree     
Br : 'a * 'a tree * 'a tree -> 'a tree

Lf is a data constructor that does not take an argument.
*)

let t1 = Br (34, Br (23, Lf, Lf), Br (54, Lf, Br (78, Lf, Lf)))

// Summing all node labels

let rec sumtree t =
    match t with 
    | Lf -> 0
    | Br (x, tl, tr) -> x + sumtree tl + sumtree tr

// sumtree t1;;

// Counting leaves in a tree

let rec count t =
    match t with 
    | Lf -> 1
    | Br (_, tl, tr) -> count tl + count tr

// count t1;;

// Preorder listing of a tree's node labels

(*
This version has quadratic complexity
since prefixes of the listing get traversed again and again.
*)

let rec preorder t = 
    match t with 
    | Lf             -> []
    | Br (x, tl, tr) -> x :: preorder tl @ preorder tr

// preorder t1;;

(*
This version is linear thanks to the use of an accumulator.
The accumulator contains the node labels that to the right
(in the preorder listing) of the given subtree in the global tree.
*)

let rec preorder' t acc =
    match t with 
    | Lf             -> acc
    | Br (x, tl, tr) -> x :: preorder' tl (preorder' tr acc)

let preordera t = preorder' t []


// Mapping over a tree

let rec mapTree f t =
     match t with
     | Lf             -> Lf 
     | Br (x, tl, tr) -> Br (f x, mapTree f tl, mapTree f tr)


// Positions in a tree

type pos =
     | S                  // the root position
     | L of pos           // a position in the left subtree
     | R of pos           // a position in the right subtree


// Extracting the subtree at a position in a tree

let rec subtree p t =
     match p, t with
     | S,   t             -> t
     | L p, Lf            -> failwith "Cannot continue"
     | L p, Br (_, t1, _) -> subtree p t1
     | R p, Lf            -> failwith "Cannot continue"
     | R p, Br (_, _, t2) -> subtree p t2



// * The option datatype

(*
The polymorphic option datatype, provided by the basis environment,
adds an "error" value to any type.

type 'a option = 
     | Some of 'a
     | None
*)


(*
With the option type, one can replace cases where a function would
naturally be undefined with cases that return a special value None.
Then undefinedness can be caught and handled.

Here's a "safe" version of the list head function.
*)

// List.head : 'a list -> 'a

// tryHead : 'a list -> 'a option

let tryHead xs =
    match xs with
    | []   -> None
    | x::_ -> Some x


(*
We can likewise make the getphone function safe.
*)

// tryGetphone : person -> int option

let tryGetphone person = 
    match person with 
    | Teacher (_, phone) -> Some phone
    | Student _          -> None

// tryGetphone (Student "Niels");;


// *** Exceptions

// * User-defined exceptions, raise and try-with

exception IllegalHour of int

(*
With this definition, we make IllegalHour an exception contructor.

IllegalHour : int -> exn
*)

(*


*)

let mins1 h = 
    if h < 0 || h > 23 then raise (IllegalHour h)
    else h * 60

// mins1 25;;


(*
In a try-with expression

try e with p -> h

expressions e, h must be of the same type,
p must be an exception pattern.
*)

// try (mins1 25) with IllegalHour _ -> -1;;



// * The Failure exception

(*
The Failure exception constructor and the function failwith
are provided by the basis environment.

exception Failure of string

let failwith s = raise (Failure s)
*)

let getphoneX person =
    match person with 
    | Teacher (_, phone) -> phone
    | Student _          -> failwith "no phone"

// getphoneX (Student "Niels");;

let mins2 h = 
    if h < 0 || h > 23 then failwith "Illegal hour"
    else h * 60

// mins2 25;;

// try mins2 25 with Failure _ -> -1;;

(*
failwithf is shorthand for failwith
where the string argument is built from a formatting spec.
*)

let mins3 h = 
    if h < 0 || h > 23 then failwithf "Illegal hour, h=%d" h
    else h * 60

// mins3 25;;

