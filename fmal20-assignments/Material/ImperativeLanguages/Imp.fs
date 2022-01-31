// T-501-FMAL Programming languages, Lecture 17
// Spring 2021

// A higher-order functional language
// (based on code by Peter Sestoft)



module Imp

(*
In an imperative language, one distinguishes between expressions
and statements (commands).

Expressions are there (primarily) to compute a value.

Commands are there to produce some side-effect, eg update mutable
store, print etc.

Below we consider a very simple imperative language
with no local names, no functions/procedures, no pointers,
no interesting types.

while z < 17 do { x = 3; y = x * 4 } ; print (y - 1) ; x = 1001

*)


// Expressions

type expr = 
    | Num of int                             // n 
    | Var of string                          // x
    | Op of string * expr * expr             // e1 op e2


// Statements (commands)

type stmt = 
    | Assign of string * expr                // x = e
    | Block of stmt list                     // {s1; ...; sn}
    | If of expr * stmt * stmt               // if e then s1 else s2       
    | For of string * expr * expr * stmt     // for x := e1 to e2 do s
    | While of expr * stmt                   // while e do s
    | Print of expr                          // print e


(*
x = 2 ; x = x + 1 ; print (x * 3)

if y then x = x + 1 else {}
*)

// Naive stores

(*
A naive store is a map from names (strings) to values (integers).

This is something we can afford so long we don't have local names.

We have no booleans, instead we represent Booleans by integers.
Any non-zero integer counts as True, zero as False.
*)

type naivestore = Map<string,int>

let emptystore : Map<string,int> = Map.empty

let getSto (sto : naivestore) x : int = sto.Item x

let setSto (sto : naivestore) (k, v) : naivestore = sto.Add (k, v)


// Evaluation of expressions

let rec eval e (sto : naivestore) : int =
    match e with
    | Num i -> i
    | Var x -> getSto sto x
    | Op (op, e1, e2) ->
        let i1 = eval e1 sto
        let i2 = eval e2 sto
        match op with
        | "*"  -> i1 * i2
        | "+"  -> i1 + i2
        | "-"  -> i1 - i2
        | "==" -> if i1 = i2 then 1 else 0
        | "<"  -> if i1 < i2 then 1 else 0
        | _    -> failwith "unknown primitive"


// Execution of statements

let rec exec stmt (sto : naivestore) : naivestore =
    match stmt with
    | Assign (x, e) -> 
        setSto sto (x, eval e sto)
    | If (e1, stmt1, stmt2) -> 
        if eval e1 sto <> 0 then exec stmt1 sto
                            else exec stmt2 sto
    | Block stmts -> 
        let rec loop stmts sto = 
            match stmts with 
            | []          -> sto
            | stmt::stmts -> loop stmts (exec stmt sto)
        loop stmts sto
(*      
    | For (x, estart, estop, stmt) ->    // this is what you usually do
        let start = eval estart sto
        let stop  = eval estop  sto
        let rec loop i sto = 
            if i > stop then sto 
                        else let sto'  = setSto sto (x, i)
                             let sto'' = exec stmt sto'
                             loop (i+1) sto''
        loop start sto
*)

(*
    | For (x, estart, estop, stmt) ->    // let's take into account
                                         // what happens to x in the loop
        let start = eval estart sto
        let stop  = eval estop  sto
        let rec loop i sto = 
            if i > stop then sto 
                        else let sto'  = setSto sto (x, i)
                             let sto'' = exec stmt sto'
                             loop (getSto sto'' x + 1) sto''
        loop start sto
*)      

    | For (x, estart, estop, stmt) ->    // let's reevaluate stop
                                         // at each iteration
        let start = eval estart sto
        let rec loop i sto =
            let stop = eval estop sto
            if i > stop then sto 
                        else let sto'  = setSto sto (x, i)
                             let sto'' = exec stmt sto'
                             loop (getSto sto'' x + 1) sto''
        loop start sto

    | While (e, stmt) -> 
        let rec loop sto =
            if eval e sto = 0 then sto
                              else loop (exec stmt sto)
        loop sto
    | Print e -> 
        printf "%d\n" (eval e sto); sto

let run stmt = 
    exec stmt emptystore
    



// Example programs

(*
{ sum = 0; for i = 0 to 100 do sum = sum + i; print sum }
*)

let ex1 =
    Block [Assign ("sum", Num 0);
          For ("i", Num 0, Num 100, 
                   Assign ("sum", Op ("+", Var "sum", Var "i")));
          Print (Var "sum")]
(*
{
  i = 1; sum = 0;
  while sum < 10000 do { print sum; sum = sum + i; i = 1 + i } ;
  print i; print sum
}     
*)

let ex2 =
    Block [Assign ("i", Num 1);
          Assign ("sum", Num 0);
          While (Op ("<", Var "sum", Num 10000),
                 Block [Print (Var "sum");
                       Assign ("sum", Op ("+", Var "sum", Var "i"));
                       Assign ("i", Op ("+", Num 1, Var "i"))]);
          Print (Var "i");
          Print (Var "sum")]

(*
{ for i = 0 to 100 do sum = sum + i; print sum }
*)

let ex1' =
    Block [For ("i", Num 0, Num 100, 
                   Assign ("sum", Op ("+", Var "sum", Var "i")));
          Print (Var "sum")]



(*
{ sum = 0;
  for i = 1 to 10 do { sum = sum + i; i = i * 2 ; print i };
  print sum
}
*)

let ex3 =
    Block [Assign("sum", Num 0);
          For ("i", Num 1, Num 10, 
                   Block [Assign ("sum", Op ("+", Var "sum", Var "i"));
                          Assign ("i", Op ("*", Var "i", Num 2));
                          Print (Var "i")
                          ]); 
          Print (Var "sum")]


(*
{ sum = 0;
  for i = 1 to 10-sum do { sum = sum + i; i = i * 2 ; print i };
  print sum
}
*)

let ex4 =
    Block [Assign("sum", Num 0);
          For ("i", Num 1, Op ("-", Num 10, Var "sum"), 
                   Block [Assign ("sum", Op ("+", Var "sum", Var "i"));
                          Assign ("i", Op ("*", Var "i", Num 2));
                          Print (Var "i")
                          ]); 
          Print (Var "sum")]
          
