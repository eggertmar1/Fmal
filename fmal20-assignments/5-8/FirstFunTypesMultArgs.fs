// T-501-FMAL Programming languages, Practice class 6
// Spring 2021
// Solutions


// Problem 5

module FirstFunTypesMultArgs

type typ =
    | Int                                // int            
    | Bool                               // bool
    | Fun of (typ list * typ)            // t1, ..., tn -> t'


type expr =
    | Var of string                      // x
    | Let of string * expr * expr        // let x = erhs in ebody
    | Call of string * expr list
                      // f e1 ... en                  
    | LetFun of string * (string * typ) list * typ * expr * expr
                      // let f (x1 : t1) ... (xn : tn) : t' = erhs in ebody
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


type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env


let rec infer (e : expr) (env : typ envir) : typ =
    match e with
    | Var x  -> 
         match lookup x env with
         | Int  -> Int
         | Bool -> Bool
         | _    -> failwith "a function used as a value"
    | Let (x, erhs, ebody) ->
         let t = infer erhs env
         let env' = (x, t) :: env
         infer ebody env'

    | Call (f, eargs) ->
         match lookup f env with
         | Fun (ts, t') ->
             if List.map (fun earg -> infer earg env) eargs = ts then t'
             else failwith "call arguments not of declared type"
         | _ -> failwith "variable called not a function"    
    | LetFun (f, xts, t', erhs, ebody) ->
         let ts = List.map snd xts
         let env' = (f, Fun (ts, t')) :: env
         let env'' = xts @ env'
         if infer erhs env'' = t' then infer ebody env'
         else failwith "letfun function body not of declared type"

    | Num i -> Int
    | Plus  (e1, e2) ->
         match infer e1 env, infer e2 env with 
         | Int, Int -> Int
         | _ -> failwith "arguments of + not integers"
    | Minus  (e1, e2) ->
         match infer e1 env, infer e2 env with 
         | Int, Int -> Int
         | _ -> failwith "arguments of - not integers"  
    | Times (e1, e2) ->
         match infer e1 env, infer e2 env with 
         | Int, Int -> Int
         | _ -> failwith "arguments of * not integers"
    | Neg e ->
         match infer e env with 
         | Int  -> Int
         | _ -> failwith "argument of negation not an integer"
    | True  -> Bool
    | False -> Bool
    | Equal (e1, e2) ->
         match infer e1 env, infer e2 env with 
         | Int, Int -> Bool
         | _ -> failwith "arguments of = not integers"
    | Less (e1, e2) ->
         match infer e1 env, infer e2 env with 
         | Int, Int -> Bool
         | _ -> failwith "arguments of < not integers"
    | ITE (e, e1, e2) ->
         match infer e env with
         | Bool -> let t1, t2 = infer e1 env, infer e2 env
                   if t1 = t2 then t1
                   else failwith "if-then-else branches of different types"
         | _ -> failwith "guard of if-then-else not a boolean"     


let rec check (e : expr) (env : typ envir) (t : typ) : unit =
    if infer e env = t then ()
    else failwith "top-level expression not of declared type"
    

// Why do we not need closures here like we do for eval?
// Because we infer and check the type of the body of a function already
// when it is defined.
// When it is called, we can already assume that it is type-correct.
// So we don't need to remember the types of the variables defined
// outside the function body.




// Here's the evaluator once again. It ignores the type annotations.

type value =
    | I of int
    | B of bool
    | F of string list * expr * value envir


let rec eval (e : expr) (env : value envir) : value =
    match e with
    | Var x  -> 
         match lookup x env with
         | I i -> I i
         | B b -> B b
         | _   -> failwith "a function used as a value"
    | Let (x, erhs, ebody) ->
         let v = eval erhs env
         let env' = (x, v) :: env
         eval ebody env'

    | Call (f, eargs) ->
         match lookup f env with
         | F (xs, ebody, env0) as clo ->
             let vs = List.map (fun earg -> eval earg env) eargs
             let xvs = List.zip xs vs
             let env' = xvs @ (f, clo) :: env0
             eval ebody env'
         | _   -> failwith "variable called not a function"     
    | LetFun (f, xts, _, erhs, ebody) ->
         let xs = List.map fst xts
         let env' = (f, F (xs, erhs, env)) :: env
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
