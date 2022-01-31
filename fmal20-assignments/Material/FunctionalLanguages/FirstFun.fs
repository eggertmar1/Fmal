// T-501-FMAL Programming languages, Lecture 9
// Spring 2021

// A first-order functional language,
// static vs dynamic scope
// (based on code by Peter Sestoft)

module FirstFun

(*
4 + 3

let x = 4 in x + 3

let x = 4 in (let y = 3 in x + y)

let f x y = x * 3 + y in f 5
*)

#nowarn "25" 

// We add function definitions and function calls to our expressions
// language.

// A function must always be named.
// An expression's value cannot be a function.

// For simplicity, a function can only take one argument.

// This language is first-order, we will get to higher-orderness
// later. 


type expr =
    | Var of string                             // x
    | Let of string * expr * expr               // let x = erhs in ebody    

    | Call of string * expr                     // f e
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


let e1 =
    LetFun ("f", "x", Plus (Var "x", Num 11),
        Call ("f", Num 3))

// let f x = x + 11 in f 3

let e2 =
    Let ("y", Num 11,
        LetFun ("f", "x", Plus (Var "x", Var "y"),
            Call ("f", Num 3)))
(*       
    let y = 11 in
        let f x = x + y in
            f 3
*)

let e3 =
    Let ("y", Num 11,
        LetFun ("f", "x", Plus (Var "x", Var "y"),
            Let ("y", Num 22,
                Call ("f", Num 3))))
(*
    let y = 11 in
        let f x = x + y in
                      ^----------------- use of y 
            let y = 22 in
                f 3 
                
Which def of y should be used here when evaluating f 3??

With static scope rule, 11.
With dynamic scope rule, 22.

A variable's definition is looked up in the current block
or the nearest enclosing block.

With static scope, this refers to syntactic nesting of blocks.

With dynamic scope, one also takes into account how
function calls nest in blocks and in each other
during evaluation. 

In this example, the use of y is in the definition of f,
which is in the body of the 3rd let (through the call of f),
which is in the body of the 2nd let,
which is in the body of the 1st let.

Static scope is the "correct" scope rule from
software engineering point of view.
Nearly all modern languages use static scope.

Dynamic scope was invented first (in Lisp).
It is more straightforward to implement than static scope.
Used in perl, which has both static and dynamic scope.
*) 


let e3' =
    LetFun ("f", "x", Plus (Var "x", Var "y"),
        Let ("y", Num 22,
           Call ("f", Num 3)))
(*
    let f x = x + y in
        let y = 22 in
            f 3

With static scope, a def of y from outside is needed,
with dynamic scope the def 22 is used. 
*)

let e4 =
    LetFun ("f", "x", LetFun ("g", "y", Plus (Var "x", Var "y"),
                          Call ("g", Times (Num 2, Var "x"))),
        Call ("f", Num 7))
// let f x = (let g y = x + y in g (2 * x)) in f 7


let e5 =
    LetFun ("f", "x", ITE (Equal (Var "x", Num 0), Num 1,
                   Times (Num 2, Call ("f", Minus (Var "x", Num 1)))),
        Call ("f", Var "y"))                                 
// let f x = if x = 0 then 1 else 2 * f (x - 1) in f y

// Functions may be recursive!




type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env



(*
A value of is an integer, boolean or a closure.
A closure is a(n anonymous) function in textual form
together with an environment 
*)

type value =
    | I of int
    | B of bool

//(*  closures for static scope - include env from def  
    | F of string * expr * value envir
                                 // (fun x -> ebody), [x1,v1; ...; xn,vn]
//*)

(* closures for dynamic scope - env not included
    | F of string * expr
*)    


(*
Evaluation works with an environment where variables
can have integer, boolean as well as function values.

But an expression's value cannot be a function.
*)

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

 
//(*    static scope version     
    | Call (f, earg) ->
         match lookup f env with
         | F (x, ebody, env0) as clo ->
             let v = eval earg env
                             // argument evaluated in current env
             let env' = (x, v) :: (f, clo) :: env0
             eval ebody env'
                             // function body evaluated
                             // in def-time environment
                             // + the value of the parameter
                             // + the value of the function name
                             //        (to handle recursion)
         | _   -> failwith "variable called not a function"     
    | LetFun (f, x, erhs, ebody) ->
         let env' = (f, F (x, erhs, env)) :: env
                             // def-time environment recorded in closure 
         eval ebody env'
//*)

// eval (Call ("f", Num 3)) ["f", F ("x", Plus (Var "x", Num 12), [])]

// eval (Let ("y", Num 85, Call ("f", Num 3))) ["f", F ("x", Plus (Var "x", Var "y"), ["y", I 1011]); "y", I 42]

// eval (Let ("y", Num 1011, LetFun ("f", "x",  Plus (Var "x", Var "y"), Let ("y", Num 85, Call ("f", Num 3))))) ["y", I 42]


(*     dynamic scope version
    | Call (f, earg) ->
         match lookup f env with
         | F (x, ebody) as clo ->
             let v = eval earg env
                             // argument evaluated in current env
             let env' = (x, v) :: (f, clo) :: env
             eval ebody env'
                             // function body evaluated
                             // in current environment
                             // + the value of the parameter
                             // + the value of the function name
                             //        (to handle recursion)
         | _   -> failwith "variable called not a function" 
    | LetFun (f, x, erhs, ebody) ->
         let env' = (f, F (x, erhs)) :: env
                             // def-time envmnt not recorded in closure 
         eval ebody env'
*)

    | Num i -> I i
    | Plus  (e1, e2) ->
         let I i1, I i2 = eval e1 env, eval e2 env in I (i1 + i2)
    | Minus  (e1, e2) ->
         let I i1, I i2 = eval e1 env, eval e2 env in I (i1 - i2)    
    | Times (e1, e2) ->
         let I i1, I i2 = eval e1 env, eval e2 env in I (i1 * i2)
    | Neg e ->
         let (I i) = eval e env in I (-i)
    | True  -> B true
    | False -> B false
    | Equal (e1, e2) ->
         let I i1, I i2 = eval e1 env, eval e2 env in B (i1 = i2)         
    | Less (e1, e2) ->
         let I i1, I i2 = eval e1 env, eval e2 env in B (i1 < i2)     
    | ITE (e, e1, e2) ->
         let (B b) = eval e env in if b then eval e1 env else eval e2 env




