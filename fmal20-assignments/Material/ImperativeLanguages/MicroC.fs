// SC-T-501-FMAL Programming languages, Lecture 18
// Spring 2021

// A C-like imperative language with pointers and arrays, "Micro-C"
// (based on code by Peter Sestoft)



module MicroC

// Types

type typ =
    | TypI                             // int
    | TypC                             // char
    | TypA of typ * int option         // array type
    | TypP of typ                      // pointer type

// Expressions, incl accessors

and expr =
                         // denote values (r-values)
                         // that are not necessarily stored
    | Access of access                 // a
    | Assign of access * expr          // a = e
    | Addr of access                   // &a
    | Num of int                       // n 
    | Op1 of string * expr             // op e
    | Op of string * expr * expr       // e1 op e2
    | Andalso of expr * expr           // e1 and e2
    | Orelse of expr * expr            // e1 or e2
    | Call of string * expr list       // f (e1, ..., en)
                                                                   
and access =             // denote addresses (l-values)
                         // and (via them) values (r-values)
                         // that have a place in the store
    | AccVar of string                 // x
    | AccDeref of expr                 // *p
    | AccIndex of access * expr        // a[e] 


// Statements (commands)

and stmt =
    | Expr of expr                     // e
    | Return of expr option            // return e
    | Block of stmtordec list          // { dec1; ...; decM; stmt1; ...; stmtN }
    | If of expr * stmt * stmt         // if (e) e1 else e2
    | While of expr * stmt             // while (e) stmt
                                                                   
and stmtordec =                                                    
    | Dec of typ * string              // t x 
    | Stmt of stmt                     // stmt


// Complete programs

and topdec = 
    | Fundec of typ option * string * (typ * string) list * stmt
                                       // type None is for "void",
                                       // type Just t for other types
    | Vardec of typ * string

and program = 
    | Prog of topdec list



// Example

let test =
    Expr (Assign (AccVar "x",
          Op ("*", Num 7,
              Assign (AccIndex (AccVar "a", Op ("+", Access (AccVar "i"), Num 5)),
               Access (AccDeref (Op ("+", Addr (AccVar "y"), Num 3)))))))



// Environments

type 'data envir = (string * 'data) list

let rec lookup env x = 
    match env with 
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup env x



// A local variable environment maps local variables to
// integer addresses (locations) and also knows the next unused
// address.

type locEnv = int envir * int


// A function environment associates to a function name
// a list of type-annotated parameters and a function body (a statement)

type paramdecs = (typ * string) list

type funEnv = (paramdecs * stmt) envir


// A global environment consists of a global variable environment 
// and a global function environment.

type gloEnv = int envir * funEnv


// A store maps integer addresses to integer values.

type address = int

type store = Map<address,int>

let emptyStore = Map.empty<address,int>

let setSto (sto : store) addr value = sto.Add (addr, value)

let getSto (sto : store) addr = sto.Item addr

let rec initSto loc n sto = 
    if n = 0 then sto else initSto (loc+1) (n-1) (setSto sto loc -999)


// Combined environment and store operations 

// Extend local variable environment so it maps x to nextloc 
// (the next store location) and set sto[nextloc] = v.

let bindVar x v (env, nextloc) sto : locEnv * store = 
    let env' = (x, nextloc) :: env 
    ((env', nextloc + 1), setSto sto nextloc v)

let rec bindVars xs vs locEnv sto : locEnv * store = 
    match xs, vs with 
    | [], []       -> locEnv, sto
    | x::xs, v::vs -> 
        let locEnv', sto' = bindVar x v locEnv sto
        bindVars xs vs locEnv' sto'
    | _ -> failwith "parameter/argument mismatch"    


// Allocate variable (int or pointer or array): extend environment so
// that it maps variable to next available store location, and
// initialize store location(s).  

let rec allocate (typ, x) (env0, nextloc) sto0 : locEnv * store = 
    let nextloc', v, sto' =
        match typ with
        | TypA (t, Some i) -> nextloc+i, nextloc, initSto nextloc i sto0
        | _ -> nextloc, -1, sto0
    bindVar x v (env0, nextloc') sto'


// Build global environment of variables and functions.  For global
// variables, store locations are reserved; for global functions, just
// add to global function environment. 

let initEnvAndStore (topdecs : topdec list) : locEnv * funEnv * store = 
    let rec addv decs locEnv funEnv sto = 
        match decs with 
        | [] -> locEnv, funEnv, sto
        | Vardec (typ, x) :: decr -> 
            let locEnv', sto' = allocate (typ, x) locEnv sto
            addv decr locEnv' funEnv sto' 
        | Fundec (_, f, xs, body) :: decr ->
            addv decr locEnv ((f, (xs, body)) :: funEnv) sto
    addv topdecs ([], 0) [] emptyStore



// Interpreting MicroC

// Evaluation of expressions


let rec eval e locEnv gloEnv sto : int * store = 
    match e with
    | Access acc      ->                                    // a
         let loc, sto' = access acc locEnv gloEnv sto
         getSto sto' loc, sto' 
    | Assign (acc, e) ->
         let loc, sto' = access acc locEnv gloEnv sto
         let res, sto'' = eval e locEnv gloEnv sto'
         res, setSto sto'' loc res 
    | Num i           -> i, sto
    | Addr acc        -> access acc locEnv gloEnv sto      // &a 
    | Op1 (op, e1)  ->
        let i1, sto' = eval e1 locEnv gloEnv sto
        let res =
            match op with
            | "!"      -> if i1 = 0 then 1 else 0
            | "printi" -> printf "%d " i1; i1
            | "printc" -> printf "%c" (char i1); i1
            | _        -> failwith ("unknown primitive " + op)
        res, sto'

    | Op (op, e1, e2) ->
        let i1, sto'  = eval e1 locEnv gloEnv sto
        let i2, sto'' = eval e2 locEnv gloEnv sto'
        let res =
            match op with
            | "*"  -> i1 * i2
            | "+"  -> i1 + i2
            | "-"  -> i1 - i2
            | "/"  -> i1 / i2
            | "%"  -> i1 % i2
            | "==" -> if i1 =  i2 then 1 else 0
            | "!=" -> if i1 <> i2 then 1 else 0
            | "<"  -> if i1 <  i2 then 1 else 0
            | "<=" -> if i1 <= i2 then 1 else 0
            | ">=" -> if i1 >= i2 then 1 else 0
            | ">"  -> if i1 >  i2 then 1 else 0
            | _    -> failwith ("unknown primitive " + op)
        res, sto'' 
    | Andalso (e1, e2) -> 
        let (i1, sto') as res = eval e1 locEnv gloEnv sto
        if i1 <> 0 then eval e2 locEnv gloEnv sto' else res
    | Orelse (e1, e2) -> 
        let (i1, sto') as res = eval e1 locEnv gloEnv sto
        if i1 <> 0 then res else eval e2 locEnv gloEnv sto'
    | Call (f, es) -> callfun f es locEnv gloEnv sto 


and access acc locEnv gloEnv sto : int * store = 
    match acc with 
    | AccVar x           -> lookup (fst locEnv) x, sto
    | AccDeref e         -> eval e locEnv gloEnv sto
    | AccIndex (acc, idx) -> 
        let a, sto' = access acc locEnv gloEnv sto
        let aval = getSto sto' a
        let i, sto'' = eval idx locEnv gloEnv sto'
        aval + i, sto'' 

and evals es locEnv gloEnv sto : int list * store = 
    match es with 
    | []     -> [], sto
    | e::es ->
        let v, sto' = eval e locEnv gloEnv sto
        let vs, sto'' = evals es locEnv gloEnv sto' 
        v::vs, sto''
    
and callfun f es locEnv gloEnv sto : int * store =
    let _, nextloc = locEnv
    let varEnv, funEnv = gloEnv
    let paramdecs, fBody = lookup funEnv f
    let vs, sto' = evals es locEnv gloEnv sto
    let fBodyEnv, sto'' = 
        bindVars (List.map snd paramdecs) vs (varEnv, nextloc) sto'
    let sto''' = exec fBody fBodyEnv gloEnv sto''
    -111, sto'''



// Execution of statements

and exec stm (locEnv : locEnv) (gloEnv : gloEnv) (sto : store) : store = 
    match stm with
    | Expr e -> 
        let _, sto' = eval e locEnv gloEnv sto 
        sto'
    | Return _ -> failwith "return not implemented"
    | Block stms -> 
        let rec loop ss (locEnv, sto) = 
            match ss with 
            | [] -> sto
            | s::ss -> loop ss (stmtordec s locEnv gloEnv sto)
        loop stms (locEnv, sto)    
    | If (e, stm1, stm2) -> 
        let v, sto' = eval e locEnv gloEnv sto
        if v <> 0 then exec stm1 locEnv gloEnv sto'
                  else exec stm2 locEnv gloEnv sto'
    | While (e, body) ->
        let rec loop sto =
            let v, sto' = eval e locEnv gloEnv sto
            if v <> 0 then loop (exec body locEnv gloEnv sto')
                    else sto'
        loop sto

and stmtordec stmtordec locEnv gloEnv sto : locEnv * store = 
    match stmtordec with 
    | Stmt stm     -> locEnv, exec stm locEnv gloEnv sto
    | Dec (typ, x) -> allocate (typ, x) locEnv sto



// Interpret a complete micro-C program by initializing the store 
// and global environments, then invoking its `main' function.

let run (Prog topdecs) vs = 
    let (varEnv, nextloc), funEnv, sto = initEnvAndStore topdecs
    let mainParams, mainBody = lookup funEnv "main"
    let mainBodyEnv, sto' = 
        bindVars (List.map snd mainParams) vs (varEnv, nextloc) sto
    exec mainBody mainBodyEnv (varEnv, funEnv) sto'



// Examples

(*

{
  int x;
  void main () {x = 12}
}  

*)

let ex1 =
    Prog  [
        Vardec (TypI, "x");
        Fundec (None, "main", [],
              Expr (Assign (AccVar "x", Num 12)))]

let ex2 =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypI, "y");
        Fundec (None, "main", [],
              Expr (Assign (AccVar "y", Num 12)))]


(*

{
  int x;
  int y;
  void main (int z) {y = z}
}  

*)

let ex3 =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypI, "y");
        Fundec (None, "main", [TypI, "z"],
              Expr (Assign (AccVar "y", Access (AccVar "z"))))]

let ex3' =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypI, "y");
        Fundec (None, "main", [TypI, "x"],
              Expr (Assign (AccVar "y", Access (AccVar "x"))))]


let ex4 =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypP TypI, "y");
        Fundec (None, "main", [],
              Expr (Assign (AccDeref (Access (AccVar "y")), Num 7)))]



let ex5 =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypP TypI, "y");
        Fundec (None, "main", [], Block [
              Stmt (Expr (Assign (AccDeref (Access (AccVar "y")), Num 7)));
              Stmt (Expr (Assign (AccVar "x", Access (AccVar "y"))))])]

let ex6 =
    Prog [
        Vardec (TypI, "x");
        Vardec (TypP TypI, "y");
        Fundec (None, "main", [], Block [
              Stmt (Expr (Assign (AccDeref (Access (AccVar "y")), Num 7)));
              Stmt (Expr (Assign (AccVar "x", Access (AccDeref (Access (AccVar "y"))))))])]


