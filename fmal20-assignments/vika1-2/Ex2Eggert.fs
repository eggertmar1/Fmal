

let rec countOcc n xs =
    match n, xs with
    | _,[]          -> 0 
    | n, x::xs      -> if n = x then 1 + countOcc n xs else countOcc n xs 



// using List.fold

(*
let countOcc y xs = List.fold (fun acc x -> if y = x then acc + 1 else acc) 0 xs
*)

// using List.filter and List.length.

(*
let countOcc y xs = List.length (List.filter ((=) y) xs)
*)


let rec occurs n xs = 
    match n, xs with 
    | _, [] -> false
    | n, x::xs -> if n = x then true else countOcct n xs 

//let countOcctrue y xs = List.fold (fun acc x -> acc || y = x) false xs


let rec sorted xs =
    match xs with 
    | [] | [_]    -> true 
    | x::y::xs -> if x <= y then sorted y::xs else false
    


let rec sorted xs =
    match xs with
    | [] | [_]         -> true
    | x::(x'::_ as xs) -> x <= x' && sorted xs


let rec pairs xs =
    match xs with
    | [] | [_]         -> []
    | x::(x'::_ as xs) -> (x, x') :: pairs xs


let rec removeFirst y xs =
    match xs with
    | []  -> []
    | x::xs -> if y = x then xs else x :: removeFirst y xs



let rec prefixes xs = 
    match xs with 
    | []            -> []
    | x::xs         -> List.map()




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
    | Lf -> Lf 
    | Br (a,l,r) ->
                    let sl = subtreeSums l
                    let sr = subtreeSums r
                    Br (a + rootLabel sl + rootLabel sr, sl, sr) 