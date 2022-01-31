// Expressions, part 2:

// Expressions with named variables, interpretation wrt an environment

// Compilation to expressions with numbered variables,
// interpretation wrt a runtime environment

// Compilation further to stack machine code,
// interpretation wrt a stack and a runtime environment


module Expressions2


// * Expressions with named variables

type expr =
    | Var of string                     // a named variable
    | Num of int
    | Op of string * expr * expr

let e1 = Num 17                        // 17
                     
let e2 = Op ("+", Num 3, Var "a")      // 3 + a

let e3 = Op ("+", Op ("*", Var "b", Num 9), Var "a")
                                        // b * 9 + a


// * Environments

// An environment is a dictionary assigning values to variables

type envir = (string * int) list

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)]

let rec lookup (x : string) (env : envir) : int =   
    match env with 
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

// lookup "c" env;;



// * Interpretation of expressions wrt an environment

let rec eval (e : expr) (env : envir) : int =
    match e with
    | Var x            -> lookup x env    
    | Num i            -> i
    | Op ("+", e1, e2) -> eval e1 env + eval e2 env
    | Op ("*", e1, e2) -> eval e1 env * eval e2 env
    | Op ("-", e1, e2) -> eval e1 env - eval e2 env
    | Op _             -> failwith "unknown primitive"

// eval e1 env;;
// eval e2 env;;
// eval e2 [("a", 314)];;
// eval e3 env;;



// * Target expressions with numbered instead of named variables

type texpr =                             
    | TVar of int                       // a number rather than a name
    | TNum of int
    | TOp of string * texpr * texpr


// Interpreting target expressions wrt a runtime environment

type renvir = int list   // values of numbered variables

let rec teval (e : texpr) (renv : renvir) : int =
    match e with
    | TVar n  -> List.item n renv     // variable n is at position n  
    | TNum i -> i
    | TOp ("+", e1, e2) -> teval e1 renv + teval e2 renv
    | TOp ("*", e1, e2) -> teval e1 renv * teval e2 renv
    | TOp ("-", e1, e2) -> teval e1 renv - teval e2 renv
    | TOp _             -> failwith "unknown primitive"


// * Compiling source expressions to target expressions 

type cenvir = string list

let rec getindex (cenv : cenvir) (x : string) : int = 
    match cenv with 
    | []      -> failwith "Variable not found"
    | y::cenv -> if x = y then 0 else 1 + getindex cenv x

let rec tcomp (e : expr) (cenv : cenvir) : texpr =
    match e with
    | Var x  -> TVar (getindex cenv x)   
    | Num i -> TNum i
    | Op (op, e1, e2) -> TOp (op, tcomp e1 cenv, tcomp e2 cenv)

// let te3 = tcomp e3 ["a";"b"];;
// teval te3 [15;6];;

// eval e3 [("b",6);("a",15)];;


(*
Correctness of compilation:

eval e env = teval (tcomp e cenv) renv 
      if for any n, List.item n renv = lookup (List.item n cenv) env
*)


// * A stack machine operating over a runtime environment

type rinstr =
  | RLoad of int            // load from environment
  | RNum of int
  | RAdd 
  | RSub
  | RMul
  | RPop
  | RDup
  | RSwap

type rcode = rinstr list

type stack = int list       // intermediate values


// * Interpreting (running) stack machine code

let rec reval (inss : rcode) (stk : stack) (renv: renvir) =
    match inss, stk  with 
    | [], i :: _ -> i
    | [], []     -> failwith "reval: no result on stack!"
    | RLoad n :: inss,             stk ->
         reval inss (List.item n renv :: stk) renv   
    | RNum i :: inss,             stk -> reval inss (i :: stk) renv
    | RAdd    :: inss, i2 :: i1 :: stk -> reval inss ((i1+i2) :: stk) renv
    | RSub    :: inss, i2 :: i1 :: stk -> reval inss ((i1-i2) :: stk) renv
    | RMul    :: inss, i2 :: i1 :: stk -> reval inss ((i1*i2) :: stk) renv
    | RPop    :: inss,        i :: stk -> reval inss stk renv
    | RDup    :: inss,        i :: stk -> reval inss ( i ::  i :: stk) renv
    | RSwap   :: inss, i2 :: i1 :: stk -> reval inss (i1 :: i2 :: stk) renv
    | _ -> failwith "reval: too few operands on stack"


// * Compiling a target expression into stack machine code

let rec rcomp (e : texpr) : rcode =
    match e with
    | TVar n            -> [RLoad n]
    | TNum i            -> [RNum i]
    | TOp ("+", e1, e2) -> rcomp e1 @ rcomp e2 @ [RAdd]
    | TOp ("*", e1, e2) -> rcomp e1 @ rcomp e2 @ [RMul]
    | TOp ("-", e1, e2) -> rcomp e1 @ rcomp e2 @ [RSub]
    | TOp _             -> failwith "unknown primitive"

// let re3 = rcomp (tcomp e3 ["a";"b"]);;
// reval re3 [] [15;6];;

// eval e3 [("b",6);("a",15)];;


(*
Correctness of compilation:

teval e renv = reval (rcomp e) [] renv
*)

