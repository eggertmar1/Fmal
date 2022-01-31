// T-501-FMAL Programming languages, Lecture 11
// Spring 2021

// A higher-order functional language
// (based on code by Peter Sestoft)


module HigherFun

(*
Why do we want higher-order functions?

They are a way to capture programming patterns in once-and-for-all
code. With them, we don't need to write the same boilerplate
over and over again. 

// map :: ('a -> 'b) -> 'a list -> 'b list
let rec map f xs = 
    match xs with 
    | []    -> []
    | x::xs -> f x :: map f xs

// filter : ('a -> bool) -> 'a list -> 'a list
let rec filter p xs =
   match xs with 
   | []    -> []
   | x::xs -> if p x then x :: filter p xs else filter p xs

// (|>) : 'a -> ('a -> 'b) -> 'b    // |> is pronounced "pipe"
let (|>) x f = f x


// f : ('a -> 'a) -> ('a -> 'a)
twice f x = f (f x)


// x |> f1 |> f2 |> .. |> fn

// swapArgs : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
let swapArgs f x y = f y x

// parComp : ('a -> 'b) * ('c -> 'd) -> 'a * 'c -> 'b * 'd
let parComp (f, g) (x, y) = (f x, g y)
*)


// We now modify the language from Lecture 9 to allow
// functions as expression values.

// This immediately gives us a higher-order language.

type expr =
    | Var of string                             // x
    | Let of string * expr * expr               // let x = erhs in ebody    

    | Call of expr * expr                       // efun earg
                        // changed! we had f earg in the first-order language

    | LetFun of string * string * expr * expr   // let f x = erhs in ebody
    | Num of int
    | Plus of expr * expr
    | Minus of expr * expr    
    | Times of expr * expr
    | Neg of expr
    | True
    | False
    | Equal of expr * expr
    | Less of expr * expr
    | ITE of expr * expr * expr    


// Some examples

let e1 = LetFun ("f1", "x", Plus (Var "x", Num 1), 
                 Call (Var "f1", Num 12))

// let f1 x = x + 1 in f1 12


let e2 = LetFun("fac", "x", 
                 ITE (Equal (Var "x", Num 0),
                    Num 1,
                    Times (Var "x", 
                              Call (Var "fac", 
                                   Minus (Var "x", Num 1)))),
                 Call (Var "fac", Var "n"))

// let fac x = if x = 0 then 1 else x * fac (x - 1) in fac n


let e3 = 
    LetFun ("tw", "g", 
           LetFun ("gg", "x", Call (Var "g", Call (Var "g", Var "x")), 
                  Var "gg"),
           LetFun ("mul3", "y", Times (Num 3, Var "y"), 
                  Call (Call (Var "tw", Var "mul3"), Num 11)))
(*
   let tw g = (let gg x = g (g x) in gg) in
   let mul3 y = 3 * y in     
   tw mul3 11
*)

let e4 = 
    LetFun ("tw", "g",
           LetFun ("gg", "x", Call (Var "g", Call (Var "g", Var "x")), 
                  Var "gg"),
           LetFun ("mul3", "y", Times (Num 3, Var "y"), 
                  Call (Var "tw", Var "mul3")))

(*
   let tw g = (let gg x = g (g x) in gg) in
   let mul3 y = 3 * y in     
   tw mul3 
*)



type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env




// A value of is an integer, boolean or a closure, like before.

// A closure is an anonymous function in textual form
// together with an environment

// But since functions are now values that can be passed around,
// in a closure we need to record the function's name
// as used in recursive calls in the rhs of the definition.


type value =
    | I of int
    | B of bool
    | F of string * string * expr * value envir
                          // (fix f -> fun x -> e), [x1,v1; ...; xn,vn]
                  // changed! we record function's name in the closure     


// As before, evaluation works with an environment where
// variables can have integer, boolean as well as function values.

// An expression's value is now also either an integer,
// boolean or a function.


let rec eval (e : expr) (env : value envir) : value =
    match e with
    | Var x  ->  lookup x env
                  // changed! we did not allow lookup to return (F _) before
                  
    | Let (x, erhs, ebody) ->
         let v = eval erhs env
         let env' = (x, v) :: env
         eval ebody env'


    | Call (efun, earg) ->
         let clo = eval efun env 
         match clo with
                 // changed! before we matched on (lookup f env) 
         | F (f, x, ebody, env0) as clo ->
             let v = eval earg env
             let env' = (x, v) :: (f, clo) :: env0
             eval ebody env'
         | _   -> failwith "expression called not a function"     
    | LetFun (f, x, erhs, ebody) ->
         let env' = (f, F (f, x, erhs, env)) :: env
         eval ebody env'

    | Num i -> I i
    | Plus  (e1, e2) ->
         match eval e1 env, eval e2 env with
         | I i1, I i2 -> I (i1 + i2)
         | _ -> failwith "argument of + not integers"
    | Minus  (e1, e2) ->
         match eval e1 env, eval e2 env with
         | I i1, I i2 -> I (i1 - i2)
         | _ -> failwith "arguments of - not integers"  
    | Times (e1, e2) ->
         match eval e1 env, eval e2 env with
         | I i1, I i2 -> I (i1 * i2)
         | _ -> failwith "arguments of * not integers" 
    | Neg e ->
         match eval e env with 
         | I i -> I (- i)
         | _ -> failwith "argument of negation not an integer"
    | True  -> B true
    | False -> B false
    | Equal (e1, e2) ->
         match eval e1 env, eval e2 env with
         | I i1, I i2 -> B (i1 = i2)
         | _ -> failwith "arguments of = not integers"      
    | Less (e1, e2) ->
         match eval e1 env, eval e2 env with
         | I i1, I i2 -> B (i1 < i2)
         | _ -> failwith "arguments of < not integers"
    | ITE (e, e1, e2) ->
         match eval e env with
         | B b -> if b then eval e1 env else eval e2 env
         | _ -> failwith "guard of if-then-else not a boolean"
