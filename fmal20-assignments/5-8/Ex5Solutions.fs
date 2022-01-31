// T-501-FMAL Programming languages, Practice class 5
// Spring 2021
// Solutions


module Ex5Solutions

// Problem 1


type rinstr =
    | RLoad of int                      // load from environment
    | RNum of int
    | RAdd 
    | RSub
    | RMul
    | RPop
    | RDup
    | RSwap

type rcode = rinstr list

type texpr =                             
    | TVar of int                       // a number rather than a name
    | TNum of int
    | TOp of string * texpr * texpr


let decomp (inss : rcode) : texpr =
    let rec decomp' inss (stk : texpr list) = 
        match inss, stk  with 
        | [], e :: _ -> e
        | [], []     -> failwith "something not right"
        | RLoad n :: inss,             stk -> decomp' inss (TVar n :: stk)   
        | RNum i :: inss,              stk -> decomp' inss (TNum i :: stk) 
        | RAdd    :: inss, e2 :: e1 :: stk -> decomp' inss (TOp ("+", e1, e2) :: stk)
        | RSub    :: inss, e2 :: e1 :: stk -> decomp' inss (TOp ("-", e1, e2) :: stk) 
        | RMul    :: inss, e2 :: e1 :: stk -> decomp' inss (TOp ("*", e1, e2) :: stk)
        | RPop    :: inss,        e :: stk -> decomp' inss stk 
        | RDup    :: inss,        e :: stk -> decomp' inss ( e ::  e :: stk) 
        | RSwap   :: inss, e2 :: e1 :: stk -> decomp' inss (e1 :: e2 :: stk) 
        | _ -> failwith "something not right"
    decomp' inss []

// Problem 2 (i)

type expr = 
    | Var of string
(*    
    | Let of string * expr * expr             // let x = erhs in ebody
*)    
    | Let of (string * expr) list * expr      // let x1 = e1, ..., xn = en in ebody     
    | Num of int  
    | Op of string * expr * expr


// (ii)

type envir = (string * int) list  // values of named variables 

let rec lookup (x : string) (env : envir) =
    match env with 
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

let rec eval (e : expr) (env : envir) : int =
    match e with
    | Var x              -> lookup x env
(*    
    | Let (x, erhs, ebody) ->            // this is simple let
         let v = eval erhs env
         let env1 = (x,v) :: env
         eval ebody env1    
*)
//(* 
    | Let (defs, ebody)  ->              // this is parallel multi-let
         let env' = List.map (fun (x, erhs) -> (x, eval erhs env)) defs
         let env1 = env' @ env 
         eval ebody env1
//*)
(* 
    | Let (defs, ebody)  ->              // this is sequential multi-let
         let rec growenv defs env =
              match defs with
              | [] -> env
              | (x, erhs)::defs' -> growenv defs' ((x, eval erhs env)::env)
         eval ebody (growenv defs env)
*)
    | Num i             -> i    
    | Op ("+", e1, e2) -> eval e1 env + eval e2 env
    | Op ("*", e1, e2) -> eval e1 env * eval e2 env
    | Op ("-", e1, e2) -> eval e1 env - eval e2 env
    | Op _             -> failwith "unknown primitive"



let test = Let (["x", Num 1],
              Let (["x", Op ("+", Var "x", Num 2); "y", Op ("*", Var "x", Num 2)],
                 Op ("*", Var "x", Var "y")))
                                  // let x = 1 in let x = x + 2, y = x * 2 in x * y   

(*

parallel:

eval (let x = 1 in let x = x + 2, y = x * 2 in x * y) []

---> eval (let x = x + 2, y = x * 2 in x * y)  [x,1]

          eval (x + 2) [x,1]     ---> 3      
          eval (x * 2) [x,1]     ---> 2

---> eval (x * y)   ([x,3; y,2]  @  [x,1])

---> 6


sequential:

eval (let x = 1 in let x = x + 2, y = x * 2 in x * y) []

---> eval (let x = x + 2, y = x * 2 in x * y)  [x,1]

          eval (x + 2) [x;1]     ---> 3      
          eval (x * 2) [x,3;x,1] ---> 6

---> eval  (x * y)  [y,6; x,3; x,1]

---> 18

*)


// (iii)

(*
Parallel multi-let:

let x1 = e1, ..., xn = en in e   |------->
    let (x1, ..., xn) = (e1, ..., en) in e


let x1 = e1, ..., xn = en in e   |------->
    let x = (e1, ..., en)
    let x1 = x.1
    ....
    let xn = x.n
    in e

Sequential multi-let:

let x1 = e1, ..., xn = en in e   |------->
    let x1 = e1 in (... (let xn = en in e))

*)


// Problem 3 (i)

let removeBlockComm (cs : char list) : char list =
    let rec removeBlockComm' cs (n : int) =
        match cs with 
        | [] -> if n = 0 then [] else failwith "unfinished comment(s)"
        | '('::'*'::cs -> removeBlockComm' cs (n+1)
        | '*'::')'::cs -> if n = 0 then failwith "unstarted comment"
                          else removeBlockComm' cs (n-1)
        | c::cs -> if n = 0 then c::removeBlockComm' cs n
                   else removeBlockComm' cs n
    removeBlockComm' cs 0              

// (ii)

let removeLineComm (cs : char list) : char list =
    let rec removeLineComm' cs (b : bool) =
        match cs with
        | [] -> []
        | '/'::'/'::cs -> removeLineComm' cs false
        | '\n'::cs -> '\n'::removeLineComm' cs true
        | c::cs -> if b then c::removeLineComm' cs b
                   else removeLineComm' cs b
    removeLineComm' cs true


// (iii)

let removeComments (cs : char list) : char list =
    let rec removeComments' cs (n : int) (b : bool) =
        match cs with 
        | [] -> if n = 0 then [] else failwith "unfinished block comment(s)"
        | '('::'*'::cs -> if b then removeComments' cs (n+1) b
                          else removeComments' cs n b 
        | '*'::')'::cs -> if b
                          then
                               if n = 0 then failwith "unstarted block comment"
                               else removeComments' cs (n-1) b
                          else removeComments' cs n b
        | '/'::'/'::cs -> if n = 0 then removeComments' cs n false
                          else removeComments' cs n b
        | '\n'::cs -> if n = 0 then '\n'::removeComments' cs n true
                      else  removeComments' cs n b
        | c::cs -> if n = 0 && b then c::removeComments' cs n b
                   else removeComments' cs n b
    removeComments' cs 0 true          



let chars2String (cs : char list) : string =
    List.fold (fun s c -> s + string c) "" cs

let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)
    
let stringVersion f s = chars2String (f (string2Chars s))
