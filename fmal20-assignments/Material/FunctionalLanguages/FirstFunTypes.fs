// T-501-FMAL Programming languages, Lecture 10
// Spring 2021

// A modified first-order functional language with type annotations
// in function definitions, type inference/checking,
// static vs dynamic typing
// (based on code by Peter Sestoft)


module FirstFunTypes


// Let's do type inference for our functional language.

// Recall that the language is first-order, i.e., expressions
// cannot take a function value. 

// We have integer, boolean and function types.

type typ =
    | Int                                // int            
    | Bool                               // bool
    | Fun of (typ * typ)                 // t -> t'

// Let us modify the language slightly so that, in letfun, types are 
// declared for the function parameter and the function body.
// This is in order to be able to infer types without "guessing".

// Adding these declarations has the side effect that 
// we in fact get a mix of type inference and checking.

type expr =
    | Var of string                      // x
    | Let of string * expr * expr
                                         // let x = erhs in ebody
    | Call of string * expr
                       // f e                  
    | LetFun of string * string * typ * typ * expr * expr
                       // let(rec) f (x : t) : t' = erhs in ebody
                       
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


let e1 =
    LetFun ("f", "x", Int, Int,  Plus (Var "x", Num 11),
        Call ("f", Num 3))

// let f (x : int) : int = x + 11 in f 3


type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env


// Rather than working with an environment of values
// for variables, type inference works with an environment
// of types.

// Type inference is quite similar to evaluation.
// It just computes types rather than values.

// However: in if-then-else, since we don't know the value
// of the guard, we don't know which branch will be taken.
// We can be sure of the type of if-then-else only when both
// branches are of the same type.
// Type inference fails if they have different types. 

// However: in function calls and letfun, we also
// check types. We infer types and compare them to the declared
// types.

// We assume the static scope rule.


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

    | Call (f, earg) ->
         match lookup f env with
         | Fun (t, t') ->
             if infer earg env = t then t'
             else failwith "call argument not of declared type"
         | _ -> failwith "variable called not a function"    
    | LetFun (f, x, t, t', erhs, ebody) ->
                   // let(rec) f (x : t) : t' = erhs in ebody
         let env' = (x, t) :: (f, Fun (t, t')) :: env
         if infer erhs env' = t' then infer ebody env' 
         else failwith "letfun function body not of declared type"

    | Num i -> Int
    | Plus  (e1, e2) ->
         match (infer e1 env, infer e2 env) with 
         | Int, Int -> Int
         | _ -> failwith "arguments of + not integers"
    | Minus  (e1, e2) ->
         match (infer e1 env, infer e2 env) with 
         | Int, Int -> Int
         | _ -> failwith "arguments of - not integers"  
    | Times (e1, e2) ->
         match (infer e1 env, infer e2 env) with 
         | Int, Int -> Int
         | _ -> failwith "arguments of * not integers"
    | Neg e ->
         match infer e env with 
         | Int  -> Int
         | _ -> failwith "argument of negation not an integer"
    | True  -> Bool
    | False -> Bool
    | Equal (e1, e2) ->
         match (infer e1 env, infer e2 env) with 
         | Int, Int -> Bool
         | _ -> failwith "arguments of = not integers"
    | Less (e1, e2) ->
         match (infer e1 env, infer e2 env) with 
         | Int, Int -> Bool
         | _ -> failwith "arguments of < not integers"
    | ITE (e, e1, e2) ->
         match infer e env with
         | Bool -> let (t1, t2) = (infer e1 env, infer e2 env)
                   if t1 = t2 then t1
                   else failwith "if-then-else branches of different types"
         | _ -> failwith "guard of if-then-else not a boolean"     


let rec check (e : expr) (env : typ envir) (t : typ) : unit =
    if infer e env = t then ()
    else failwith "top-level expression not of declared type"
    

// Why are closures not needed here like they were for eval?
// Because we infer and check the type of the body of a function already
// when it is defined.
// When it is called, we can already assume that it is type-correct.
// So we don't need to remember the types of the variables defined
// outside the function body.

(*
A language is said to be statically typed (better: have static
type-checking) if a program is checked for type-correctness at compile-time.
Type-correct programs should not give errors at runtime, but this is not
quite true (think division by 0, or out-of-bounds array access).

A language is dynamically typed if it has types but no static type-checking.

Why is static typing good?
*)

(*
Lisp, Scheme, ECMAScript/Javascript, Perl, Postscript, Python, Ruby
are dynamically typed.

Java and C# are for the most part statically typed, but not fully.

Eg dynamic typing in Java and C# reference-type array assignment,
eg this code in Java compiles but gives a run-time error.

Integer[] arr = new Integer[16];
Number[] arrN = arr;

arrN[0] = new Double(3.14);
*) 



// Here's the evaluator once again. It just ignores the type annotations.

type value =
    | I of int
    | B of bool
    | F of string * expr * value envir


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

    | Call (f, earg) ->
         match lookup f env with
         | F (x, ebody, env0) as clo ->
             let v = eval earg env
             let env' = (x, v) :: (f, clo) :: env0
             eval ebody env'
         | _   -> failwith "variable called not a function"     
    | LetFun (f, x, _, _, erhs, ebody) ->
         let env' = (f, F (x, erhs, env)) :: env
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


