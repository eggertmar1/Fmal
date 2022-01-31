// T-501-FMAL Programming languages, Lecture 6
// Spring 2021

// Expressions, part 3
// (based on code by Peter Sestoft)

// Let-bindings

module Expressions3


// Expressions with variables and let-bindings


// We had global variables before in Expressions2,
// now we add local variables.

type expr = 
    | Var of string
    | Let of string * expr * expr          // let x = erhs in ebody
    | Num of int  
    | Op of string * expr * expr


// Some closed expressions

let e1 = Let ("z", Num 17, Op ("+", Var "z", Var "z"))
                // let z = 17 in z + z 

let e2 = Let("z", Num 17, 
             Op ("+", Let ("z'", Num 22, Op ("*", Var "z", Var "z'")),
                       Var "z"))
                // let z = 17 in (let z' = 22 in z * z') + z  

let e3 = Let("z", Op ("-", Num 5, Num 4), 
             Op ("*", Num 100, Var "z"))
                // let z = 5 - 4 in 100 * z 

let e4 = Op ("+", Op("+", Num 20, Let ("z", Num 17, 
                                          Op ("+", Var "z", Num 2))),
                   Num 30)
               // (20 + let z = 17 in z + 2) + 30   

let e5 = Op ("*", Num 2, Let ("x", Num 3, Op ("+", Var "x", Num 4)))
               // 2 * let x = 3 in x + 4


// * Interpretation of expressions wrt an environment

type envir = (string * int) list  // values of named variables 

let rec lookup (x : string) (env : envir) =
    match env with 
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

let rec eval (e : expr) (env : envir) : int =
    match e with
    | Var x              -> lookup x env 
    | Let (x, erhs, ebody) -> 
         let xval = eval erhs env
         let env1 = (x, xval) :: env 
         eval ebody env1
                // the environment is extended by an additional  binding
    | Num i             -> i    
    | Op ("+", e1, e2) -> eval e1 env + eval e2 env
    | Op ("*", e1, e2) -> eval e1 env * eval e2 env
    | Op ("-", e1, e2) -> eval e1 env - eval e2 env
    | Op _             -> failwith "unknown primitive";;

let run e = eval e []


// * Closed expressions

// let contains x xs = List.exists (fun y -> x = y) xs

let rec contains x xs = 
    match xs with
    | []      -> false
    | y :: xs -> x = y || contains x xs

// closedin checks that the free variables are contained in a given list.

let rec closedin (e : expr) (xs : string list) : bool =
    match e with
    | Var x -> List.exists (fun y -> x = y) xs
    | Let (x, erhs, ebody) -> 
          let xs1 = x :: xs 
          closedin erhs xs && closedin ebody xs1
    | Num i -> true      
    | Op (op, e1, e2) -> closedin e1 xs && closedin e2 xs

// An expression is closed if there are no free variables.

let closed e = closedin e []



// * Target expressions with numbered instead of named variables

type texpr =                             
    | TVar of int
    | TLet of texpr * texpr
                        // new var always gets number 0
    | TNum of int
    | TOp of string * texpr * texpr


// * Interpreting (evaluating) target expressions wrt a runtime environment

type renvir = int list  // values of numbered variables

let rec teval (e : texpr) (renv : renvir) : int =
    match e with
    | TVar n  -> List.item n renv     // variable n is at position n
    | TLet (erhs, ebody) -> 
        let xval = teval erhs renv
        let renv1 = xval :: renv      
        teval ebody renv1 
    | TNum i -> i
    | TOp ("+", e1, e2) -> teval e1 renv + teval e2 renv
    | TOp ("*", e1, e2) -> teval e1 renv * teval e2 renv
    | TOp ("-", e1, e2) -> teval e1 renv - teval e2 renv
    | TOp _             -> failwith "unknown primitive"


// * Compiling source expressions to target expressions 

type cenvir = string list

let rec getindex (cenv : 'a list) (x : 'a) : int = 
    match cenv with 
    | []      -> failwith "Variable not found"
    | y::cenv -> if x = y then 0 else 1 + getindex cenv x

let rec tcomp (e : expr) (cenv : cenvir) : texpr =
    match e with
    | Var x  -> TVar (getindex cenv x)
    | Let (x, erhs, ebody) ->         
        let cenv1 = x :: cenv 
        TLet (tcomp erhs cenv, tcomp ebody cenv1)
                         // new var gets number 0
                         // old var numbers are increased by 1
    | Num i -> TNum i
    | Op (op, e1, e2) -> TOp (op, tcomp e1 cenv, tcomp e2 cenv)


(*
Correctness of compilation:

eval e env = teval (tcomp e cenv) renv 
      if for any n, List.item n renv = lookup (List.item n cenv) env
*)


// * A stack machine
// with a stack for intermediate values
// and another for variable-bound values

type rinstr =
    | RLoad of int            // load from environment
    | RStore                  // move value from top of stack to
                              // 0th pos of the environment,
                              // shifting all else down
    | RErase                  // remove 0th value from environment,
                              // shifting all else up                        
    | RNum of int
    | RAdd 
    | RSub
    | RMul
    | RPop
    | RDup
    | RSwap

type rcode = rinstr list

type stack = int list         // intermediate values

// * Interpreting (running) stack machine code

let rec reval (inss : rcode) (stk : stack) (renv : renvir) =
    match inss, stk with 
    | [], i :: _ -> i
    | [], []     -> failwith "reval: No result on stack!"
    | RLoad n :: inss,             stk ->
          reval inss (List.item n renv :: stk) renv
    | RStore  :: inss,        i :: stk -> reval inss stk (i :: renv)
    | RErase  :: inss,             stk -> reval inss stk (List.tail renv) 
    | RNum i  :: inss,             stk -> reval inss (i :: stk) renv
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
    | TVar n             -> [RLoad n]
    | TLet (erhs, ebody) -> rcomp erhs @ [RStore] @ rcomp ebody @ [RErase]
    | TNum i             -> [RNum i]
    | TOp ("+", e1, e2)  -> rcomp e1 @ rcomp e2 @ [RAdd]
    | TOp ("*", e1, e2)  -> rcomp e1 @ rcomp e2 @ [RMul]
    | TOp ("-", e1, e2)  -> rcomp e1 @ rcomp e2 @ [RSub]
    | TOp _              -> failwith "unknown primitive"




// * A different stack machine with intermediate and bound values
// kept in a single stack

type sinstr =
    | SNum of int 
    | SAdd   
    | SSub 
    | SMul 
    | SPop                               // discard a variable
    | SDup of int                        // load a variable
    | SSwap

type scode = sinstr list


// * Evaluating stack machine code

let rec seval (inss : scode) (stk : stack) =
    match inss, stk with
    | [], i :: _ -> i
    | [], []     -> failwith "seval: no result on stack"
    | SNum i  :: inss,         stk -> seval inss (i :: stk) 
    | SAdd    :: inss, i2::i1::stk -> seval inss (i1+i2 :: stk)
    | SSub    :: inss, i2::i1::stk -> seval inss (i1-i2 :: stk)
    | SMul    :: inss, i2::i1::stk -> seval inss (i1*i2 :: stk)
    | SPop    :: inss,    _ :: stk -> seval inss stk
    | SDup n  :: inss,         stk -> seval inss (List.item n stk :: stk)     
    | SSwap   :: inss, i2::i1::stk -> seval inss (i1::i2::stk)
    | _ -> failwith "seval: too few operands on stack"


// * Compiling source expessions to stack machine code

// The compile-time environment now keeps track of
// which positions of the stack contain intermediate values,
// and which contain variable-bound values.

let rec scomp (e : expr) (cenv : (string option) list) : scode =
    match e with
    | Var x -> [SDup (getindex cenv (Some x))]
    | Let (x, erhs, ebody) -> 
          scomp erhs cenv @ scomp ebody (Some x :: cenv) @ [SSwap; SPop]
    | Num i -> [SNum i]         
    | Op ("+", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (None :: cenv) @ [SAdd] 
    | Op ("-", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (None :: cenv) @ [SSub] 
    | Op ("*", e1, e2) -> 
          scomp e1 cenv @ scomp e2 (None :: cenv) @ [SMul] 
    | Op _ -> failwith "scomp: unknown operator"



// Write a similar compiler for target expressions!!