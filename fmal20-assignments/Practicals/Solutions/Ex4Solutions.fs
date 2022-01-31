// T-501-FMAL Programming languages, Practice class 4
// Spring 2021
// Solutions

module Ex4Solutions

// Problem 1 (i)

type expr =
    | Var of string
    | Num of int
    | Plus  of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Quot  of expr * expr
    | Rem   of expr * expr
    | Neg of expr

// Plus (Var "x", Num 3);;


// (ii)

let rec prettyprint (e : expr) : string =
    match e with
    | Var x -> x
    | Num i -> string i
    | Plus  (e1, e2) -> "(" + prettyprint e1 + " + " + prettyprint e2 + ")" 
    | Minus (e1, e2) -> "(" + prettyprint e1 + " - " + prettyprint e2 + ")"
    | Times (e1, e2) -> "(" + prettyprint e1 + " * " + prettyprint e2 + ")"
    | Quot  (e1, e2) -> "(" + prettyprint e1 + " / " + prettyprint e2 + ")"
    | Rem   (e1, e2) -> "(" + prettyprint e1 + " % " + prettyprint e2 + ")"
    | Neg e -> "(-" + prettyprint e + ")"


let rec lookup (x : string) env =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

let rec eval (e : expr) (env : (string * int) list) : int =
    match e with
    | Var x -> lookup x env
    | Num i -> i
    | Plus  (e1, e2) -> eval e1 env + eval e2 env
    | Minus (e1, e2) -> eval e1 env - eval e2 env
    | Times (e1, e2) -> eval e1 env * eval e2 env
    | Quot  (e1, e2) -> eval e1 env / eval e2 env
    | Rem   (e1, e2) -> eval e1 env % eval e2 env
    | Neg e -> - eval e env

let e1 = Times (Plus (Neg (Num 3), Var "x"), Var "y")

let e2 = Neg (Neg (Quot (Num 4, Var "z")))

let e = Times (e1, e2)

// prettyprint e;;
// eval e ["x", 5; "y", 2; "z", 7];;


// Problem 2

let paren b s = if b then  "(" + s + ")" else s

let rec prettyprint2 (e : expr) (acc : int) : string =
    match e with
    | Var x -> x
    | Num i -> string i
    | Plus  (e1, e2) ->
         paren (4 <= acc) (prettyprint2 e1 3 + " + " + prettyprint2 e2 4) 
    | Minus (e1, e2) ->
         paren (4 <= acc) (prettyprint2 e1 3 + " - " + prettyprint2 e2 4)
    | Times (e1, e2) ->
         paren (7 <= acc) (prettyprint2 e1 6 + " * " + prettyprint2 e2 7)
    | Quot  (e1, e2) ->
         paren (7 <= acc) (prettyprint2 e1 6 + " / " + prettyprint2 e2 7)
    | Rem   (e1, e2) ->
         paren (7 <= acc) (prettyprint2 e1 6 + " % " + prettyprint2 e2 7)   
    | Neg e ->
         paren (10 <= acc) ("-" + prettyprint2 e 9)

// prettyprint2 e 0;;

(*
Plus (Num 100, Plus (Num 120, Num 130))   -----> "100 + (120 + 130)"

Plus (Plus (Num 100, Num 120), Num 130)   -----> "100 + 120 + 130"

100 - (120 - 130) is not the same as 100 - 120 - 130! 
*)

// Problem 3 (i)

// I am omitting -, /, % for brevity

type xexpr =
    | XVar of string
    | XNum of int
    | XPlus of xexpr * xexpr
    | XTimes of xexpr * xexpr
    | XNeg of xexpr
    | True
    | False
    | Equal of xexpr * xexpr
    | Less of xexpr * xexpr
    | And of xexpr * xexpr
    | Or of xexpr * xexpr
    | Not of xexpr
    | ITE of xexpr * xexpr * xexpr    


// (ii)

type value =
    | I of int
    | B of bool

let rec xeval (e : xexpr) (env : (string * int) list) : value =
    match e with
    | XVar x  -> I (lookup x env)
    | XNum i -> I i
    | XPlus  (e1, e2) ->
         match xeval e1 env, xeval e2 env with
         | I i1, I i2 -> I (i1 + i2)
         | _          -> failwith "wrong operand type" 
    | XTimes (e1, e2) ->
         match xeval e1 env, xeval e2 env with
         | I i1, I i2 -> I (i1 * i2)
         | _          -> failwith "wrong operand type"
    | XNeg e ->
         match xeval e env with
         | I i -> I (- i)
         | _   -> failwith "wrong operand type"
    | True -> B true
    | False -> B false
    | Equal (e1, e2) ->
         match xeval e1 env, xeval e2 env with
         | I i1, I i2 -> B (i1 = i2)
         | _          -> failwith "wrong operand type"
    | Less (e1, e2) ->
         match xeval e1 env, xeval e2 env with
         | I i1, I i2 -> B (i1 < i2)
         | _          -> failwith "wrong operand type"
(*
    | And   (e1, e2) ->                             // eager &&
         match xeval e1 env, xeval e2 env with
         | B b1, B b2 -> B (b1 && b2)
         | _          -> failwith "wrong operand type"
*)
    | And   (e1, e2) ->                             // shortcircuit &&
         match xeval e1 env with
         | B true  ->
              match xeval e2 env with
              | B b2  -> B b2
              | _     -> failwith "wrong operand type"  
         | B false ->  B false
         | _       -> failwith "wrong operand type"
(*                                                  // eager ||
    | Or   (e1, e2) ->
         match xeval e1 env, xeval e2 env with
         | B b1, B b2 -> B (b1 || b2)
         | _          -> failwith "wrong operand type"
*)
    | Or   (e1, e2) ->                              // shortcircuit ||
         match xeval e1 env with
         | B true  -> B true
         | B false ->
              match xeval e2 env with
              | B b2  -> B b2
              | _     -> failwith "wrong operand type"  
         | _       -> failwith "wrong operand type"
    | Not e ->
         match xeval e env with
         | B b -> B (not b)
         | _   -> failwith "wrong operand type"
    | ITE (e, e1, e2) ->
         match xeval e env with
         | B true  -> xeval e1 env
         | B false -> xeval e2 env
         | _       -> failwith "wrong operand type"



(*
A shorter version with inexhaustive patterns (gives warnings)

let rec xeval (e : xexpr) (env : (string * int) list) : value =
    match e with
    | XVar x  -> I (lookup x env)
    | XNum i -> I i
    | XPlus  (e1, e2) ->
         let (I i1, I i2) = (xeval e1 env, xeval e2 env) in I (i1 + i2)
    | XTimes (e1, e2) ->
         let (I i1, I i2) = (xeval e1 env, xeval e2 env) in I (i1 * i2)
    | XNeg e ->
         let (I i) = xeval e env in I (- i)
    | True -> B true
    | False -> B false
    | Equal (e1, e2) ->
         let (I i1, I i2) = (xeval e1 env, xeval e2 env) in B (i1 = i2)         
    | Less (e1, e2) ->
         let (I i1, I i2) = (xeval e1 env, xeval e2 env) in B (i1 < i2)     
(*
    | And   (e1, e2) ->                             // eager &&
         let (B b1, B b2) = (xeval e1 env, xeval e2 env) in B (b1 && b2)
*)
    | And   (e1, e2) ->                             // shortcircuit &&
         let (B b1) = xeval e1 env
         if b1 then let (B b2) = xeval e2 env in B b2 else B false
(*                                                  // eager ||
    | Or    (e1, e2) ->
         let (B b1, B b2) = (xeval e1 env, xeval e2 env) in B (b1 || b2) 
*)
    | Or    (e1, e2) ->                             // shortcircuit ||
         let (B b1) = xeval e1 env
         if b1 then B true else let (B b2) = xeval e2 env in B b2
    | Not e ->
         let (B b) = xeval e env in B (not b)
    | ITE (e, e1, e2) ->
         let (B b) = xeval e env in if b then xeval e1 env else xeval e2 env
*)



// Problem 4

let rec simpl (e : xexpr) : xexpr =
    match e with
                                     // interesting cases
    | And (e1, e2)    -> ITE (simpl e1, simpl e2, False)
    | Or  (e1, e2)    -> ITE (simpl e1, True, simpl e2)
    | Not e           -> ITE (simpl e,  False, True)
                                     // boring cases
    | XPlus (e1, e2)  -> XPlus (simpl e1, simpl e2)
    | XTimes (e1, e2) -> XTimes (simpl e1, simpl e2)
    | XNeg e          -> XNeg (simpl e)
    | Equal (e1, e2)  -> Equal (simpl e1, simpl e2)
    | Less (e1, e2)   -> Less (simpl e1, simpl e2)
    | ITE (e, e1, e2) -> ITE (simpl e, simpl e1, simpl e2)
    | e               -> e          // cases XVar, XNum, True, False


// Problem 5

type typ =
    | Int
    | Bool

let rec infer (e : xexpr) : typ =
    match e with
    | XVar x -> Int 
    | XNum i -> Int
    | XPlus  (e1, e2) ->
         match infer e1, infer e2 with
         | Int,  Int  -> Int 
         | _          -> failwith "wrong operand type"
(*
    | XPlus  (e1, e2) -> let Int, Int = infer e1, infer e2 in Int
*)
    | XTimes (e1, e2) ->
         match infer e1, infer e2 with
         | Int,  Int  -> Int
         | _          -> failwith "wrong operand type"
    | XNeg e ->
         match infer e with
         | Int -> Int
         | _   -> failwith "wrong operand type"
    | True  -> Bool
    | False -> Bool
    | Equal (e1, e2) ->
         match infer e1, infer e2 with
         | Int,  Int  -> Bool
         | _          -> failwith "wrong operand type"
    | Less (e1, e2) ->
         match infer e1, infer e2 with
         | Int,  Int  -> Bool
         | _          -> failwith "wrong operand type"
    | And   (e1, e2) ->
         match infer e1, infer e2 with
         | Bool, Bool -> Bool
         | _          -> failwith "wrong operand type"
    | Or   (e1, e2) ->
         match infer e1, infer e2 with
         | Bool, Bool -> Bool
         | _          -> failwith "wrong operand type"
    | Not e ->
         match infer e with
         | Bool -> Bool
         | _    -> failwith "wrong operand type"
    | ITE (e, e1, e2) ->
         match infer e, infer e1, infer e2 with
         | Bool, Int,  Int  -> Int
         | Bool, Bool, Bool -> Bool
         | Bool, _,    _    -> failwith "branches of different types"
         | Int,  _,    _    -> failwith "guard not boolean"

