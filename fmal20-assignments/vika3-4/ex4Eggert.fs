type expr = 
    | Var of string 
    | Num of int 
    | Op of string * expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Neg of expr
    | Times of expr * expr
    | Quot of expr *  expr // quotient
    | Rem of expr * expr // remainder



    

type envir = (string * int) list

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)]

let rec lookup (x : string) (env : envir) : int =   
    match env with 
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env


    

let rec eval (e : expr) (env : (string * int) list) : int =
    match e with
    | Var x             -> lookup x env    
    | Num i             -> i
    | Plus (e1, e2)     -> eval e1 env + eval e2 env
    | Minus (e1, e2)    -> eval e1 env - eval e2 env
    | Times (e1, e2)    -> eval e1 env * eval e2 env
    | Quot (e1, e2)     -> eval e1 env / eval e2 env
    | Rem (e1, e2)      -> eval e1 env % eval e2 env
    | Neg e             -> - eval e env



let e1 = Times (Plus (Neg (Num 3), Var "x"), Var "y")