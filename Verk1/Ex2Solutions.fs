// T-501-FMAL Programming languages, Practice class 2
// Spring 2021
// Solutions

module  Ex2Solutions


// Problem 1 (i)


// countOcc directly by recursion

let rec countOcc y xs =
     match xs with
     | []    -> 0
     | x::xs -> if y = x then 1 + countOcc y xs else countOcc y xs

(*
let countOcc y xs = List.foldBack (fun x n -> if y = x then 1 + n else n) xs 0
*)


// using List.fold

(*
let countOcc y xs = List.fold (fun acc x -> if y = x then acc + 1 else acc) 0 xs
*)

// using List.filter and List.length.

(*
let countOcc y xs = List.length (List.filter ((=) y) xs)
*)

// the same as a "pipe"

(*
let countOcc y xs = xs |> List.filter ((=) y) |> List.length
*)

// or differently

(*
let countOcc y xs = xs |> (List.filter ((=) y) >> List.length)
*)



// (ii)

// occurs using countOcc

(*
let occurs y xs = countOcc y xs > 0
*)

// using List.filter

(*
let occurs y xs = not (List.isEmpty (List.filter ((=) y) xs))
*)

// directly by recursion

let rec occurs y xs =
    match xs with
    | []    -> false
    | x::xs -> y = x || occurs y xs

(*
let occurs y xs = List.foldBack (fun x b -> y = x || b) xs false
*)

// using List.fold

(*
let occurs y xs = List.fold (fun acc x -> acc || y = x) false xs
*)

// using List.exists

(*
let occurs y xs = List.exists ((=) y) xs
*)

// occurs written with the help of countOcc always traverses the whole list,
// be countOcc coded directly by recursion or using List.fold,
// since countOcc must necessarily traverse the whole list to
// compute the count.

// occurs written by directly by recursion or using List.exists only
// traverses the list up to the position where y is found.

// [Annoyingly though, if the direct recursive definition of
// occurs is reformulated in terms of List.foldBack, then the
// whole list is traversed. This is because function application
// is strict in F#, so evaluation of (fun x b -> y = x || b) x' b'
// requires evaluation of b' even when y = x' is true. 
// In a nonstrict language like Haskell, the definition of occurs with
// foldBack (called foldr in Haskell) only traverses the list up to
// the position where y is found.]

// occurs coded with List.fold traverses the whole list
// since the only base case for fold is []. 


// Problem 2

// sorted directly by recursion

let rec sorted xs =
    match xs with
    | [] | [_]         -> true
    | x::(x'::_ as xs) -> x <= x' && sorted xs


// using pairs and List.fold

let rec pairs xs =
    match xs with
    | [] | [_]         -> []
    | x::(x'::_ as xs) -> (x, x') :: pairs xs

(*
let sorted xs = List.fold (fun acc (x, x') -> acc && x <= x') true (pairs xs)
*)

// using pairs and List.forall

(*
let sorted xs = List.forall (fun (x, x') -> x <= x') (pairs xs)
*)

// Problem 3 (i)

let rec removeFirst y xs =
    match xs with
    | []  -> []
    | x::xs -> if y = x then xs else x :: removeFirst y xs

// (ii)

let rec remove y xs =
    match xs with
    | []  -> []
    | x::xs -> if y = x then remove y xs else x :: remove y xs

// (iii)

let rec nub xs =
    match xs with
    | []  -> []
    | x::xs -> x :: nub (remove x xs)

// Problem 4 (i)

let rec suffixes xs =
    xs :: match xs with
          | []    -> []
          | _::xs -> suffixes xs


// (ii)

let rec prefixes xs =
    [] :: match xs with
          | []    -> []
          | x::xs -> List.map (fun ys -> x :: ys) (prefixes xs)

// (iii)

let rec sublists xs =
    match xs with
    | [] -> [[]]
    | x::xs -> let yss = sublists xs
               List.map (fun ys -> x :: ys) yss @ yss

(*
let rec sublists xs =
    match xs with
    | [] -> [[]]
    | x::xs -> List.collect (fun ys -> [x :: ys; ys]) (sublists xs)
*)

// Problem 5 (i)

type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree

let rootLabel t =
    match t with
    | Lf           -> 0      // leaves are not labelled, but we can pretend
                             // that they carry label 0
    | Br (n, _, _) -> n

let rec subtreeSums t =
    match t with
    | Lf          -> Lf
    | Br (a, l, r) ->
         let sl = subtreeSums l
         let sr = subtreeSums r
         Br (a + rootLabel sl + rootLabel sr, sl, sr)
            
// (ii)

let pathSums t =
    let rec pathSums' acc t =  
        match t with
        | Lf           -> Lf
        | Br (a, l, r) ->
             let acc' = acc + a
             Br (acc', pathSums' acc' l, pathSums' acc' r)
    pathSums' 0 t            

