// T-501-FMAL, Spring 2021, Assignment 1

(*
STUDENT NAMES HERE: Eggert Mar Eggertsson


*) 

//module Assignment1

// Problem 1

// nf : int -> int

let rec nf n = 
  match n with
  | 1 -> 2 
  | _ when n < 1 -> 1
  | _ -> 2 * nf(n-1) + 3 * nf(n-2) 

// Problem 2

// (i)

// lastTrue : (int -> bool) -> int -> int
let rec lastTrue f n =
  match f(n-1), n with
    | _,_ when n < 1 -> -1
    | true,_ -> (n-1) 
    | false,_ -> lastTrue f (n-1)  
   



// (ii)

// lastEqual : 'a -> (int -> 'a) -> int -> int when 'a : equality
let rec lastEqual x f n = 
  lastTrue ( fun n -> f n = x) n  
    

// (iii)

// firstTrue : (int -> bool) -> int -> int
let rec firstTrue f n = 
  let rec counter f n x =
    match f(x), n with
    | _,_ when n < 1 -> -1
    | _,_ when x > n -> -1
    | true,_ -> x
    | _,_ -> counter f n (x + 1)
  counter f n 0
  



// (iv)

// If  lastTrue (fun x -> f x > f (x + 1)) 100  evaluates to -1,
// what can you say about  f?

(*
ANSWER 2(iv)(a) HERE: ...

f x can not be bigger than f (x+1) so that automatically is false. 

*)

// How about if  lastTrue f 100 = firstTrue f 100  is  true?

(*
ANSWER 2(iv)(b) HERE: ...
number in range is 99 

*)


// Problem 3

// repeat_map : ('a -> 'a) -> 'a list -> 'a list

let rec repeat_map f xs = 
  match xs with
    | [] -> []
    | x::xs -> f x::repeat_map f (List.map f xs)

// Problem 4

// (i)

// sum_some : int option list -> int

let rec sum_some xs =
  match xs with 
  | [] -> 0
  | x::xs -> if x<>None then x.Value + sum_some xs else sum_some xs 



// (ii)  (uncomment the definition below when you've completed it)


let sum_some2 xs =
    List.fold (fun s o ->
        match o with
        | None -> s
        | Some x -> s + x
        ) 0 xs
     



// (iii)  (uncomment the definition below when you've completed it)


let sum_some3 xs =
    let f o = 
      match o with 
      | None -> 0
      | Some x -> x
    List.fold (+) 0 (List.map f xs)



// Problem 5

type 'a nelist =
  | One of 'a
  | Cons of 'a * 'a nelist


// (i)

// ne_product : int nelist -> int
let rec ne_product xs = 
  Match xs wit
  | one x
  | cons x (cons (xx xxx))
  | cons (x one xx)

// (ii)

// ne_append : 'a nelist -> 'a nelist -> 'a nelist



// (iii)

// to_list : 'a nelist -> 'a list

let to_list (xs : 'a nelist) : 'a list = failwith "to implement" 


// (iv)

// ne_map : ('a -> 'b) -> 'a nelist -> 'b nelist


// (v)

// to_pair : 'a nelist -> 'a * 'a list

let to_pair xs =
    match xs with 
    | One x -> (x, [])
    | Cons (x, xs) -> (x, to_list xs)

// from_pair : 'a * 'a list -> 'a nelist



// (vi)

// Is it possible to write a function  from_list : 'a list -> 'a nelist
// such that the expressions  to_list (from_list xs) = xs
// and  from_list (to_list ys) = ys  evaluate to  true?
// Explain why.

(*
ANSWER 5(vi) HERE: ...


*)


// Problem 6

type product_tree =
  { value: int
  ; children: product_tree list
  ; product: int option }

// (i)

// are_same : product_tree -> product_tree -> bool


// (ii)

// get_product : product_tree -> int



// (iii)

// fill_products : product_tree -> product_tree
