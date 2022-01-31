module Assignment1Solutions

// Problem 1 

// withinBounds : int -> int -> int list -> bool
let rec withinBounds min max xs =
    match xs with
    | []    -> true
    | x::xs -> min <= x && x <= max && withinBounds min max xs


// Problem 2

// (i)

// findSum : int -> int list -> int
let rec findSum sum xs =
    if sum = 0 then 0
    else
        match xs with
        | []    -> 0
        | x::xs -> 1 + findSum (sum - x) xs

// (ii)

// findSum2 : int -> int list -> int
let findSum2 sum xs =
    let (_, n) =
        List.fold
            (fun (r, i) x -> if r = 0 then (r, i) else (r - x, i + 1))
            (sum, 0)
            xs
    n


// Problem 3 

// isBracketed : int list -> bool
let isBracketed xs =
    let rec isBracketed' sum xs =
        match xs with
        | []    -> sum = 0
        | x::xs -> sum + x >= 0 && isBracketed' (sum + x) xs
    isBracketed' 0 xs


// Problem 4 

// lookup : 'a -> string -> (string * 'a) list -> 'a
let rec lookup d (k : string) dict =
    match dict with
    | [] -> d
    | (k', v) :: dict -> if k = k' then v else lookup d k dict

// update : string -> 'a -> (string * 'a) list -> (string * 'a) list
let rec update (k : string) v dict =
    match dict with
    | [] -> [(k, v)]
    | (k', _) as p :: dict ->
        if k = k' then (k, v) :: dict else p :: update k v dict

// (i)

// count : string -> string list -> int
let rec count (x : string) xs =
    match xs with
    | [] -> 0
    | y :: xs ->
        let c = count x xs
        if x = y then 1 + c else c

// (ii)

// modify : 'a -> ('a -> 'a) -> string -> (string * 'a) list -> (string * 'a) list
// let modify d f x dict = update x (f (lookup d x dict)) dict

let rec modify d f (x : string) dict =
    match dict with
    | [] -> [(x, f d)]
    | (y, v) :: dict ->
        if x = y then (x, f v) :: dict else (y, v) :: modify d f x dict

// (iii)

// ac : (string * int) list -> string list -> (string * int) list
let rec ac dict xs =
    match xs with
    | [] -> dict
    | x :: xs ->
        let dict' = modify 0 (fun c -> c + 1) x dict
        ac dict' xs


// Problem 5 

// uf : ('b -> ('a * 'b) option) -> 'b -> 'a list
let rec uf f x =
    match f x with
    | None -> []
    | Some (a, y) -> a :: uf f y

// fromOne : int -> int list
let fromOne n = uf (fun x -> if x > n then None else Some (x, x + 1)) 1


// Problem 6 

type 'a tree =
    | Lf
    | Br of 'a * 'a tree * 'a tree
type pos =
    | S
    | L of pos
    | R of pos

// deleteSubtree : 'a tree -> pos -> 'a tree
let rec deleteSubtree t p =
    match t, p with
    | Lf, _             -> Lf
    | Br (_, _, _), S   -> Lf
    | Br (x, l, r), L p -> Br (x, deleteSubtree l p, r)
    | Br (x, l, r), R p -> Br (x, l, deleteSubtree r p)


// Problem 7 

type fp =
    | Nil
    | IntCons of int * fp
    | StrCons of string * fp

// (i)

// fromIntList : int list -> fp
let rec fromIntList xs =
    match xs with
    | [] -> Nil
    | x :: xs -> IntCons (x, fromIntList xs)

// (ii)

// extractInts : fp -> int list
let rec extractInts l =
    match l with
    | Nil -> []
    | IntCons (x, l) -> x :: extractInts l
    | StrCons (_, l) -> extractInts l

// (iii)

// valid : fp -> bool
let rec valid l =
    match l with 
    | Nil | IntCons (_, Nil) | StrCons (_, Nil) -> true
    | IntCons (_, IntCons (_, _)) -> false
    | StrCons (_, StrCons (_, _)) -> false
    | IntCons (_, l) | StrCons (_, l) -> valid l

// (iv)

// norm : fp -> fp
let rec norm l =
    match l with 
    | Nil -> Nil
    | IntCons (n, IntCons (m, l)) -> norm (IntCons (n + m, l))
    | StrCons (s, StrCons (t, l)) -> norm (StrCons (s + t, l))
    | IntCons (n, l) -> IntCons (n, norm l)
    | StrCons (s, l) -> StrCons (s, norm l)







let rec group2 = function 
      | []              -> []
      | [x1]            -> []
      | x1::x2::xs      -> [x1;x2] :: group3 xs


let rec group3 = function 
    | []            -> []
    | [x1]          -> []
    | [x1;x2]       -> []
    | x1::x2::x3::xs -> [x1;x2;x3] :: group2 xs 


let group23 xs = (group2 xs) 



let rec safeFind func xs = 
    match xs with 
    | []         -> None
    | x1::xs     -> match func x1 with 
                    | true -> Some x1
                    | false -> safeFind func xs 