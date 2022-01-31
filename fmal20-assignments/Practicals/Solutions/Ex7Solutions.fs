// T-501-FMAL Programming languages, Practice class 7
// Spring 2021
// Solutions


module Ex7Solutions


// Problem 1

let htmlrow (n : int, ss : int -> string) =
    let rec tds i =
         if i = n then "" else "<td>" + string (ss i) + "</td>" + tds (i+1)
    "<tr>" + tds 0 + "</tr>"


let htmltable (n : int, ss : int -> string) =
    let rec trs i =
         if i = n then "" else string (ss i) + "\n" + trs (i+1)
    "<table>\n" + trs 0 + "</table>"

(*
htmltable (3, fun i -> "<tr><td>" + string i + "</td><td>" +
                                  string (i * 8) + "</td></tr>");;

htmltable (10,
           fun i -> htmlrow (10,
                             fun j -> string ((i + 1) * (j + 1))));;
*)


// Problem 2

open HigherFun

let e1 =
    LetFun ("add", "x", LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"),
        Call (Call (Var "add", Num 2), Num 5))

let e2 = 
    LetFun ("add", "x", LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"),
        Let ("addtwo", Call (Var "add", Num 2),
            Call (Var "addtwo", Num 5)))

let e3 = 
    LetFun ("add", "x", LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"),
        Let ("addtwo", Call (Var "add", Num 2),
            Let ("x", Num 77, Call (Var "addtwo", Num 5))))

let e4 = 
    LetFun ("add", "x",
        LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"),
            Call (Var "add", Num 2))

// eval e1 [];;
// val it : value = I 7

// eval e2 [];;
// val it : value = I 7

// eval e3 [];;
// val it : value = I 7

// eval e4 [];;
// val it : value =
//  F ("f","y",Plus (Var "x",Var "y"),
//     [("x", I 2);
//      ("add", F ("add","x",LetFun ("f","y",Plus (Var "x",Var "y"),Var "f"),[]))])



// Problem 3

let rec scopeCheck (e : expr) (env : unit envir) : unit =
    match e with
    | Var x  ->  lookup x env

    | Let (x, erhs, ebody) ->
         scopeCheck erhs env; scopeCheck ebody ((x, ()) :: env)

    | Call (efun, earg) ->
         scopeCheck efun env; scopeCheck earg env        
    | LetFun (f, x, erhs, ebody) ->
         let env' = (f, ()) :: env
         scopeCheck erhs ((x, ()) :: env'); scopeCheck ebody env'

    | Num i -> ()
    | Plus  (e1, e2) ->
         scopeCheck e1 env; scopeCheck e2 env
    | Minus  (e1, e2) ->
         scopeCheck e1 env; scopeCheck e2 env
    | Times (e1, e2) ->
         scopeCheck e1 env; scopeCheck e2 env
    | Neg e ->
         scopeCheck e env
    | True  -> ()
    | False -> ()
    | Equal (e1, e2) ->
         scopeCheck e1 env; scopeCheck e2 env
    | Less (e1, e2) ->
         scopeCheck e1 env; scopeCheck e2 env
    | ITE (e, e1, e2) ->
         scopeCheck e env; scopeCheck e1 env; scopeCheck e2 env


// scopeCheck (Plus (Var "x", Var "y")) [];;
// System.Exception: x not found

// scopeCheck (Plus (Var "x", Var "y")) ["x", (); "y", ()];;
// val it : unit = ()

// scopeCheck (Let ("x", Num 5, Plus (Var "x", Var "y"))) ["y", ()];;
// val it : unit = ()


// Problem 4

(*

type expr =
    | ...
    | Fun of string * expr


type value =
    | ...
    | NRF of string * expr * value envir    // fun x -> e, [x1,v1;...;xn,vn]


let rec eval (e : expr) (env : value envir) : value =
    match e with
    ...

    | Call (efun, earg) ->
         let clo = eval efun env 
         match clo with
         | F (f, x, ebody, env0) ->
             let v = eval earg env
             let env' = (x, v) :: (f, clo) :: env0
             eval ebody env'
         | NRF (x, ebody, env0) ->
             let v = eval earg env
             let env' = (x, v) :: env0
             eval ebody env'    
         | _ -> failwith "expression called not a function"  
    | Fun (x, e) -> NRF (x, e, env)
    ...

*)


// Problem 5

(*
let e5 =
    LetFun ("add", "x", LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"),
        Var "add")

let e6 =
    LetFun ("add", "x", Fun ("y", Plus (Var "x", Var "y")),
        Var "add")

let e7 =
    Fun ("x", Fun ("y", Plus (Var "x", Var "y")))
*)

// eval e5 [];;
// val it : value =
//  F ("add","x",LetFun ("f","y",Plus (Var "x",Var "y"),Var "f"),[])

// eval e6 [];;
// val it : value = F ("add","x",Fun ("y",Plus (Var "x",Var "y")),[])

// eval e7 [];;
// val it : value = NRF ("x",Fun ("y",Plus (Var "x",Var "y")),[])


// eval (Call (e5, Num 2)) [];;
// val it : value =
//  F ("f","y",Plus (Var "x",Var "y"),
//     [("x", I 2);
//      ("add", F ("add","x",LetFun ("f","y",Plus (Var "x",Var "y"),Var "f"),[]))])

// eval (Call (e6, Num 2)) [];;
// val it : value =
//  NRF
//    ("y",Plus (Var "x",Var "y"),
//     [("x", I 2); ("add", F ("add","x",Fun ("y",Plus (Var "x",Var "y")),[]))])

// eval (Call (e7, Num 2)) [];;
// val it : value = NRF ("y",Plus (Var "x",Var "y"),[("x", I 2)])

