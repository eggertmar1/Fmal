// T-501-FMAL Programming languages, Practice class 8
// Spring 2021


// Problem 3, higher-order functions with pairs:
// expressions, values, evaluation


module HigherFunPairs

// Expressions

type expr =
    | Var of string                             // x
    | Let of string * expr * expr               // let x = erhs in ebody    

    | Call of expr * expr                       // efun earg
    | LetFun of string * string * expr * expr   // let(rec) f x = erhs in ebody

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
    
    | MkPair of expr * expr                     // (e1, e2)  NEW!
    | Fst of expr                               // fst e1    NEW!
    | Snd of expr                               // snd e2    NEW!



// Environments

type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env




// Values

type value =
    | I of int
    | B of bool
    | F of string * string * expr * value envir

    | P of value * value                               // NEW!           


// Evaluation


let rec eval (e : expr) (env : value envir) : value =
    match e with
    | Var x  ->  lookup x env
                  
    | Let (x, erhs, ebody) ->
         let v = eval erhs env
         let env' = (x, v) :: env
         eval ebody env'


    | Call (efun, earg) ->
         let clo = eval efun env 
         match clo with
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
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 + i2)
         | _ -> failwith "argument of + not integers"
    | Minus  (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 - i2)
         | _ -> failwith "arguments of - not integers"  
    | Times (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 * i2)
         | _ -> failwith "arguments of * not integers" 
    | Neg e ->
         match eval e env with 
         | I i -> I (- i)
         | _ -> failwith "argument of negation not an integer"
    | True  -> B true
    | False -> B false
    | Equal (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> B (i1 = i2)
         | _ -> failwith "arguments of = not integers"      
    | Less (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> B (i1 < i2)
         | _ -> failwith "arguments of < not integers"
    | ITE (e, e1, e2) ->
         match eval e env with
         | B b -> if b then eval e1 env else eval e2 env
         | _ -> failwith "guard of if-then-else not a boolean"

    | MkPair (e1, e2) ->                               // NEW!
         P (eval e1 env, eval e2 env)
    | Fst e ->                                         // NEW
         match eval e env with
         | P (v1, _) -> v1
         | _ -> failwith "argument of fst not a pair"  
    | Snd e ->
         match eval e env with                         // NEW!
         | P (_, v2) -> v2
         | _ -> failwith "argument of snd not a pair"
                 
         