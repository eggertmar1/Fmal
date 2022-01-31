type 'a tree = 
    | Lf 
    | Br of 'a * 'a tree * 'a tree 
   
type 'a netree = 
    | N of 'a * ('a netree) option * ('a netree) option

//tree to netree, replaces lf with None and Br with Some
let rec tree2netree t = 
    match t with 
    | Lf -> None
    | Br (x, l, r) -> Some (N (x, tree2netree l, tree2netree r))


let rec netree2tree t = 
    match t with 
    | N (x, None, None)         -> Br (x, Lf , Lf)
    | N (x, Some l, None)       -> Br (x, netree2tree l, Lf)
    | N (x, None, Some r)       -> Br (x, Lf, netree2tree r)
    | N (x, Some l, Some r)     -> Br (x, netree2tree l, netree2tree r)

type person = 
    { name : string
    ; father : person option
    ; mother : person option
    }
   
let optionmap f mba = 
    match mba with 
    | None  -> None 
    | Some a -> Some(f a)


let rec person2netree { name = n; father = f; mother = m}=
    N (n, optionmap person2netree f, optionmap person2netree m)


let rec netree2person (N (name,left,right)) = 
    { name = name 
    ; father = optionmap netree2person left 
    ; mother = optionmap netree2person right 
    }

let rec truncate d t = 
    match d, t with
    | 0, _     -> Lf
    | _, Lf    -> Lf
    | d, Br (x, l, r) -> let d = d - 1
                         Br (x, truncate d l, truncate d r)

let rec indent d =
    match d with
    | 0 -> ""
    | n -> indent (n-1) + "    "


// prettyprint : 'a tree -> unit

let rec prettyprint' d t =
    match t with
    | Lf           -> printf "%s.\n" (indent d) 
    | Br (x, l, r) -> printf "%s%A\n" (indent d) x
                      prettyprint' (d+1) l;
                      prettyprint' (d+1) r
                      
let prettyprint t = prettyprint' 0 t




// 1 pound (L) = 20 s
// 1 shilling (s) = 12 d
// 1 penny (d) = 4 farthings (f) 
type oldCurrency = 
    | Lsdf of (int * int * int * int)

let normalize (Lsdf (l, s, d, f)) =
    let d' = d + f / 4
    let s' = s + d' / 12
    let l' = l + s' / 20 
    Lsdf (l', s' % 20, d' % 12, f % 4)

let (+++) (Lsdf (l1, s1, d1, f1)) (Lsdf (l2, s2, d2, f2)) =
    normalize (Lsdf (l1 + l2, s1 + s2, d1 + d2, f1 + f2))

type oldCoin =
    | Farthing
    | Halfpenny
    | Penny
    | Threepence
    | Sixpence
    | Shilling
    | Florin
    | HalfCrown
    | Crown
    | DoubleFlorin

let value c =
    match c with
    | Farthing      -> Lsdf (0, 0, 0, 1)
    | Halfpenny     -> Lsdf (0, 0, 0, 2)
    | Penny         -> Lsdf (0, 0, 1, 0)
    | Threepence    -> Lsdf (0, 0, 3, 0)
    | Sixpence      -> Lsdf (0, 0, 6, 0)
    | Shilling      -> Lsdf (0, 1, 0, 0)
    | Florin        -> Lsdf (0, 2, 0, 0)
    | HalfCrown     -> Lsdf (0, 2, 6, 0)
    | DoubleFlorin  -> Lsdf (0, 4, 0, 0)
    | Crown         -> Lsdf (0, 5, 0, 0)

let totalValue cs =
    List.fold (fun lsd c -> lsd +++ value c) (Lsdf (0, 0, 0, 0)) cs 


// 1 pound (L) = 100 new pence (p)

type decCurrency = 
    | Lp of (int * float) 


let roundtohalves x =
    let y = round (2.0 * x)
    y / 2.0 

let old2dec (Lsdf (l, s, d, f)) =
    Lp (l, float (5 * s) + roundtohalves (5.0 / 48.0 * float (4 * d + f)))


// Problem 7

// complex numbers

type cmplx =
    | C of float * float

let (.+) (C (x, y)) (C (z, w)) = C (x + z, y + w)

let (.*) (C (x, y)) (C (z, w)) = C (x * z - y * w, x * w + y * z)


(*
type cmplx = { re : float; im : float }

let (++) { re : x ; im : y } { re : z ; im : w } =
    { re : x + z ; im : y + w}

let (**) { re : x ; im : y } { re : z ; im : w } =
    { re : x * z - y * w ; im : x * w + y * z }
*)




