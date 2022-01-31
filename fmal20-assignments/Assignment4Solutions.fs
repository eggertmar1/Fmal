// SC-T-501-FMAL Programming languages, Assignment 4 
// Spring 2020
// Solutions


module Assignment4

// Problem 1

// Write Yes or No in each cell of the table.
//
//                   | (a)           | (b)               | (c)
//                   | all X. X -> X | (all X. X) ->     | (all X. X -> X) ->
//                   |               |        (all X. X) |      (all X. X -> X)
// ------------------+---------------+-------------------+---------------------
// (i)   \x. x       | Yes           | Yes               | Yes
// (ii)  \x. x x     | No            | Yes               | Yes
// (iii) \f. \x. f x | No            | No                | Yes



// Definitions for use in Problems 2-3

// Expressions

type expr = 
    | CstI of int
    | Var of string
    | Prim of string * expr * expr

// Statements (aka commands)

type stmt = 
    | Assign of string * expr                // x = e
    | CAssign of string * expr               // x ?= e    
    | Block of stmt list
    | If of expr * stmt * stmt  
    | For of string * expr * expr * stmt
    | While of expr * stmt
    | Print of expr


// Problem 2

let sumFactors : stmt =
  Block
    [Assign ("r",CstI 0);
     For
       ("i", CstI 1, Var "n",
        For
          ("j", CstI 1, Var "n",
           If
             (Prim ("==",Prim ("*",Var "i",Var "j"),Var "n"),
              Assign ("r",Prim ("+",Var "r",Var "i")),Block [])))]

// In C syntax, this is
//   r = 0;
//   for (i = 1; i <= n; i++) {
//     for (j = 1; j <= n; j++) {
//       if (i * j == n) {
//         r = r + i;
//       }
//     }
//   }


// Further definitions for use in Problem 3

// Naive stores

type naivestore = Map<string,int>

let emptystore : Map<string,int> = Map.empty

let getSto (store : naivestore) x = store.Item x

let setSto (store : naivestore) (k, v) = store.Add(k, v)


// Evaluation of expressions

let rec eval e (store : naivestore) : int =
    match e with
    | CstI i -> i
    | Var x  -> getSto store x
    | Prim (op, e1, e2) ->
        let i1 = eval e1 store
        let i2 = eval e2 store
        match op with
        | "*"  -> i1 * i2
        | "+"  -> i1 + i2
        | "-"  -> i1 - i2
        | "==" -> if i1 = i2 then 1 else 0
        | "<"  -> if i1 < i2 then 1 else 0
        | "??" -> if i1 = 0 then 0 else i2
        | _    -> failwith "unknown primitive"


// Problem 3

// Execution of statements

let rec exec stmt (store : naivestore) : naivestore =
    match stmt with
    | Assign (x, e) -> 
        setSto store (x, eval e store)
    | CAssign (x, e) ->
        // SOLUTION: The critical case is here.
        if getSto store x = 0 then setSto store (x, eval e store) else store
    | If (e1, stmt1, stmt2) -> 
        if eval e1 store <> 0 then exec stmt1 store
                              else exec stmt2 store
    | Block stmts -> 
        let rec loop ss sto = 
            match ss with 
            | []     -> sto
            | s::ss -> loop ss (exec s sto)
        loop stmts store
    | For (x, estart, estop, stmt) -> 
        let start = eval estart store
        let stop  = eval estop  store
        let rec loop i sto = 
            if i > stop then sto 
                        else loop (i+1) (exec stmt (setSto sto (x, i)))
        loop start store
    | While (e, stmt) -> 
        let rec loop sto =
            if eval e sto = 0 then sto
                              else loop (exec stmt sto)
        loop store
    | Print e -> 
        printf "%d\n" (eval e store); store

let run stmt = 
    exec stmt emptystore


// Problem 4

let rec removeCAssign stmt =
  match stmt with
  | Assign _ -> stmt
  | CAssign (x, e) -> If (Prim ("==", Var x, CstI 0), Assign (x, e), Block [])
  | Block stmts -> Block (List.map removeCAssign stmts)
  | If (e, stmt1, stmt2) -> If (e, removeCAssign stmt1, removeCAssign stmt2)
  | For (x, estart, estop, stmt) -> For (x, estart, estop, removeCAssign stmt)
  | While (e, stmt) -> While (e, removeCAssign stmt)
  | Print _ -> stmt


// Problem 5

// Prettyprinted, this statement looks like this:
// x = (7 * (a[i + 5] = *(&y + 3)))

// Or with fewer parentheses:
// x = 7 * (a[i + 5] = *(&y + 3))


// Problem 6

//   (i) f(0, 10) prints  2 2
//  (ii) f(1, 10) prints 10 1
// (iii) g(5, 6)  prints 10 6
//  (iv) g(1, 2)  prints  2 2


// Test cases for Problem 2

let execSumFactors n = getSto (exec sumFactors (setSto emptystore ("n", n))) "r"
// > execSumFactors 1;;
// val it : int = 1
// > execSumFactors 2;;
// val it : int = 3
// > execSumFactors 3;;
// val it : int = 4
// > execSumFactors 4;;
// val it : int = 7
// > execSumFactors 5;;
// val it : int = 6
// > execSumFactors 6;;
// val it : int = 12

// Test cases for Problem 3

// > run (Block [Assign ("x", CstI 0); CAssign ("x", CstI 10)]);;
// val it : naivestore = map [("x", 10)]
// > run (Block [Assign ("x", CstI 1); CAssign ("x", CstI 10)]);;
// val it : naivestore = map [("x", 1)]
// > run (Block [Assign ("x", CstI 1); CAssign ("x", CstI 0)]);;
// val it : naivestore = map [("x", 1)]
// > run (Block [Assign ("x", CstI 0); For ("i", CstI 1, CstI 10, CAssign ("x", Prim ("+", Var "x", CstI 1)))]);;
// val it : naivestore = map [("i", 10); ("x", 1)]

