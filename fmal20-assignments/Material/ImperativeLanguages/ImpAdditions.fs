module ImpAdditions


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

                                             // NEW!
    | Repeat of stmt * expr                  // repeat s until e


// Naive stores

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

    | Repeat (stm, e) ->                          // NEW!
         let rec loop sto =
            let sto' = exec stm sto 
            if eval e sto' <> 0 then sto'
                                else loop sto'
         loop sto


let run stmt = 
    exec stmt emptystore
    

