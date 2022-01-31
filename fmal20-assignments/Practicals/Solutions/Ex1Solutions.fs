// T-501-FMAL Programming languages, Practice class 1
// Spring 2021
// Solutions


module  Ex1Solutions 


// Problem 1

(*
let f = fun n -> n * 3 + 8
*)

let f n = n * 3 + 8

// f 5;;

(*
let g = fun m -> f (m - 1)
*)

let g m = f (m - 1)

// g 21;;


// Problem 2

(*
Here SML complains that the pow2 in the rhs is not in scope.
Rightly so. This is definition is incorrect. 

let pow2 = fun n -> if n = 0 then 1 else 2 * pow2 (n - 1)
*)

(*
let rec pow2 = fun n -> if n = 0 then 1 else 2 * pow2 (n - 1)
*)

(*
let rec pow2 n = if n = 0 then 1 else 2 * pow2 (n - 1)
*)

let rec pow2 n =
    match n with
    | 0 -> 1
    | _ -> 2 * pow2 (n - 1)

// pow2 7;;
// pow2 8;;

(*
// pow2 (-3);;
This is infinite recursion, so leads to stack overflow.
*)


// Problem 3

// This is McCarthy's "91 function".

let rec mc n = if n > 100 then n - 10 else mc (mc (n + 11))

// mc 73;;
// mc 84;;
// mc 92;;
// mc 95;;
// mc 109;;
// mc 117;;
// mc 125;;


// Problem 4

let rec feq (h : int -> int) k n m =
       if n > m then true else h n = k n && feq h k (n + 1) m

(*
Without the type declaration h : int -> int,
SML infers that both h, k can be of type int -> 'a,
but also concludes that 'a must support equality testing.
This is also fine.
*)

// feq mc (fun n -> 91) 1 100;;

// feq mc (fun n -> n - 10) 101 200;;


// Problem 5

let rec groups3 = function
    | []                   -> []
    | [x1]                 -> [[x1]]
    | [x1; x2]             -> [[x1; x2]]
    | x1 :: x2 :: x3 :: xs -> [x1; x2; x3] :: groups3 xs

// groups3 [1;5;3;4;3;5;5;7];;


// Problem 6 (i)

let rec take n xs =
    match n, xs with 
    | 0, xs      -> []
    | n, []      -> []
    | n, x :: xs -> x :: take (n - 1) xs

let rec drop n xs =
    match n, xs with
    | 0, xs      -> xs
    | n, []      -> []
    | n, x :: xs -> drop (n - 1) xs


(*
let takeDrop n xs = (take n xs, drop n xs)

This function traverses xs twice, once for taking, once for dropping.
*)

let rec takeDrop n xs =
    match n, xs with 
    | 0, xs      -> [], xs
    | n, []      -> [], []
    | n, x :: xs -> let t, d = takeDrop (n - 1) xs
                    x :: t, d


// takeDrop 4 [3;2;5;9;4;6;8;0;1];;
// takeDrop 6 [3;2;5;9;4];;


// (ii)

(*
let rec groupsN n xs =
    match n, xs with 
    | n, [] -> [] 
    | n, xs -> match takeDrop n xs with
               | t, d -> t :: groupsN n d
*)

let rec groupsN n xs =
    match (n, xs) with 
    | n, [] -> [] 
    | n, xs -> let t, d = takeDrop n xs
               t :: groupsN n d

// groupsN 4 [3;2;5;9;4;6;8;0;1];;

