// T-501-FMAL, Spring 2021, Assignment 4

(*
STUDENT NAMES HERE: Eggert Mar Eggertsson


*)

//module Assignment4

// Problem 1

(*
ANSWER 1 HERE:
    (i) g(1, 2) prints 1, h(1, 2) prints 2
  

   (ii) g(1, 0) prints 0, h(1, 0) prints 0
 

  (iii) g(0, 0) prints 0, h(0, 0) prints 50
        when we return U, the *t = 50, and then we print y ( *t)
*)


// Abstract syntax
type expr =
    | Access of access            // a
    | Addr of access              // &a
    | Num of int                  // n
    | Op of string * expr * expr  // e1 op e2
and access =
    | AccVar of string            // x
    | AccDeref of expr            // *p
and stmt =
    | Alloc of access * expr        // p = alloc e
    | Print of expr               // print e
    | Assign of access * expr       // p = e
    | TestAndSet of expr * expr   // test_and_set(p, q)
    | Call of string * expr list  // f(e1, ..., en)
    | Block of stmt list          // { stmt1; ...; stmtN }
    | If of expr * stmt * stmt    // if (e) e1 else e2
    | While of expr * stmt        // while (e) stmt
and fundec =
    string *                      // function name
    string list *                 // argument names
    string list *                 // local variable names
    stmt                          // function body
and program =
    | Prog of fundec list

// Examples of concrete and abstract syntax

// void main () {
//   var p;
//   p = alloc(2);
//   *(p + 1) = 11;
//   print(*(p + 1));
// }
let ex =
  ("main", [], ["p"], Block [
    Alloc (AccVar "p", Num 2);
    Assign (AccDeref (Op ("+", Access (AccVar "p"), Num 1)), Num 11);
    Print (Access (AccDeref (Op ("+", Access (AccVar "p"), Num 1))))
  ])

// void make_range(dest_p, lower, upper) {
//   var i;
//   *dest_p = alloc((upper - lower) + 1);
//   while (lower <= upper) {
//     *((*dest_p) + i) = lower;
//     i = i + 1;
//     lower = lower + 1;
//   }
// }
let make_range =
  ("make_range", ["dest_p"; "lower"; "upper"], ["i"], Block [
    Alloc (AccDeref (Access (AccVar "dest_p")), Op ("+", Op ("-", Access (AccVar "upper"), Access (AccVar "lower")), Num 1));
    While (Op ("<=", Access (AccVar "lower"),  Access (AccVar "upper")), Block [
      Assign (AccDeref (Op ("+", Access (AccDeref (Access (AccVar "dest_p"))), Access (AccVar "i"))), Access (AccVar "lower"));
      Assign (AccVar "i", Op ("+", Access (AccVar "i"), Num 1));
      Assign (AccVar "lower", Op ("+", Access (AccVar "lower"), Num 1))
    ])
  ])



// Problem 2

// void print_array(a, length) {
//   var i;
//   while (i < length) {
//     print(*(a + i));
//     i = i + 1;
//   }
// }
let print_array =
  ("print_array", ["a"; "length"], ["i"], Block [
    While (Op ("<", Access ( AccVar "i"), Access ( AccVar "length")), Block [
      Print( Access (AccDeref (Op ("+", Access ( AccVar "a"), Access ( AccVar "i")))))
      Assign ( AccVar "i", Op ("+", Access ( AccVar "i"), Num 1));
    ])
  ])

// void memcpy(dest, src, length) {
//   while (length) {
//     *dest = *src;
//     dest = dest + 1;
//     src = src + 1;
//     length = length - 1;
//   }
// }
let memcpy =
  ("memcpy", ["dest"; "src"; "length"], [], Block [
    While ( Op (">", Access ( AccVar "length"), Num 0), Block [
      Assign ( AccDeref (Access ( AccVar "dest")), Access (AccDeref ( Access ( AccVar "src"))));  
      Assign ( AccVar "dest", Op ( "+", Access (AccVar "dest"), Num 1));
      Assign ( AccVar "src", Op ( "+", Access (AccVar "src"), Num 1));
      Assign ( AccVar "length", Op ( "-", Access (AccVar "length"), Num 1));

    ])
  ])

// void make_copy(dest_p, src, length) {
//   *dest_p = alloc(length);
//   memcpy(*dest_p, src, length);
// }
//   *dest_p = alloc((upper - lower) + 1);


let make_copy =
    ("make_copy", ["dest_p"; "src"; "length"], [], Block [
        Alloc(AccDeref(Access(AccVar "dest_p")), Access(AccVar "length"))
        Call("memcpy", [Access(AccDeref(Access(AccVar "dest_p"))); Access(AccVar("src")); Access(AccVar("length"))])
    ])

// Problem 3


// (i)
// void array_to_list(dest_p, a, length) {
//   var cur;
//   *dest_p = 0
//   while (length) {
//       length = length - 1;
//       cur = alloc(2);
//       *cur = *a + length; 
//       (*cur + 1) = *dest_p;
//       *dest_p = *cur; 
//  }
// }
let array_to_list =
  ("array_to_list", ["dest_p"; "a"; "length"], ["cur"], Block [
    Assign (AccDeref (Access (AccVar "dest_p")), Num 0);
    While (Access (AccVar "length"), Block [
      Assign (AccVar "length", Op ("-", Access (AccVar "length"), Num 1));
      Alloc (AccVar "cur", Num 2);
      Assign (AccDeref (Access (AccVar "cur")), Access (AccDeref (Op ("+", Access (AccVar "a"), Access (AccVar "length")))));
      Assign (AccDeref (Op ("+", Access (AccVar "cur"), Num 1)), Access (AccDeref (Access (AccVar "dest_p"))));
      Assign (AccDeref (Access (AccVar "dest_p")), Access (AccVar "cur"))
    ])
  ])

// (ii)
let print_list =
  ("print_list", ["l"], ["cur"], Block [
    Assign ( AccVar "cur", Access ( AccVar "l"));
    While (Access ( AccVar "cur"), Block [
      Print ( Access ( AccDeref ( Access ( AccVar "cur"))))
      Assign ( AccVar "cur", Op ( "+", Access (AccVar "cur"), Num 1));
      Assign ( AccVar "cur", Access ( AccDeref ( Access(  AccVar "cur")))); 
    ])
  ])



// Various definitions used in the interpreter

type 'data envir = (string * 'data) list
let rec lookup env x =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup env x

type locEnv = int envir * int
type funEnv = (string list * string list * stmt) envir

type address = int
type store = Map<address,int> * int
let emptyStore = (Map.empty<address,int>, 1)
let setSto ((map, nextloc) : store) addr value = map.Add (addr, value), nextloc
let getSto ((map, nextloc) : store) addr = map.Item addr
let allocSto ((map, nextloc) : store) length =
  let r = nextloc
  let nextloc' = r + length
  let map' = List.fold (fun (m : Map<address,int>) addr -> m.Add (addr, 0)) map [r..(nextloc' - 1)]
  let sto' = map', nextloc'
  sto', r

let bindVar x v (env, nextloc) sto : locEnv * store =
    let env' = (x, nextloc) :: env
    ((env', nextloc - 1), setSto sto nextloc v)
let rec bindVars xs vs locEnv sto : locEnv * store =
    match xs, vs with
    | [], []       -> locEnv, sto
    | x::xs, v::vs ->
        let locEnv', sto' = bindVar x v locEnv sto
        bindVars xs vs locEnv' sto'
    | _ -> failwith "parameter/argument mismatch"

let initFunEnv (fundecs : fundec list) : funEnv =
    let rec addv decs funEnv =
        match decs with
        | [] -> funEnv
        | (f, parameters, locals, body) :: decr ->
            addv decr ((f, (parameters, locals, body)) :: funEnv)
    addv fundecs []



// Interpreter
let rec eval (e : expr) (locEnv : locEnv) (funEnv : funEnv) (sto : store) : int =
    match e with
    | Access acc      -> getSto sto (access acc locEnv funEnv sto)
    | Num i           -> i
    | Addr acc        -> access acc locEnv funEnv sto
    | Op (op, e1, e2) ->
        let i1 = eval e1 locEnv funEnv sto
        let i2 = eval e2 locEnv funEnv sto
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
and access acc locEnv funEnv (sto : store) : int =
    match acc with
    | AccVar x   -> lookup (fst locEnv) x
    | AccDeref e -> eval e locEnv funEnv sto
and evals es locEnv funEnv (sto : store) : int list =
    List.map (fun e -> eval e locEnv funEnv sto) es
and callfun f es locEnv (funEnv : funEnv) (sto : store) : store =
    let _, nextloc = locEnv
    let paramNames, localNames, fBody = lookup funEnv f
    let arguments = evals es locEnv funEnv sto
    // local variables are initialized to 0
    let localValues = List.map (fun _ -> 0) localNames
    let fBodyEnv, sto' = bindVars (paramNames @ localNames) (arguments @ localValues) ([], nextloc) sto
    exec fBody fBodyEnv funEnv sto'



// Problem 4

and exec stm (locEnv : locEnv) (funEnv : funEnv) (sto : store) : store =
    match stm with
    | Print e ->
        let res = eval e locEnv funEnv sto
        printf "%d " res;
        sto
    | Call (f, es) -> callfun f es locEnv funEnv sto
    | Assign (acc, e) ->
        let loc = access acc locEnv funEnv sto
        let res = eval e locEnv funEnv sto
        setSto sto loc res
    // | TestAndSet (p, q) ->
    //     let t = access p locEnv funEnv sto 
    //     let e = access q locEnv funEnv sto 
    //     //here 
    | Alloc (acc, e) ->
        let loc = access acc locEnv funEnv sto
        let n = eval e locEnv funEnv sto
        let sto', res = allocSto sto n
        setSto sto' loc res
    | Block stms ->
        List.fold (fun sto' s -> exec s locEnv funEnv sto') sto stms
    | If (e, stm1, stm2) ->
        let v = eval e locEnv funEnv sto
        if v <> 0 then exec stm1 locEnv funEnv sto
                  else exec stm2 locEnv funEnv sto
    | While (e, body) ->
        let rec loop sto =
            let v = eval e locEnv funEnv sto
            if v <> 0 then loop (exec body locEnv funEnv sto)
                    else sto
        loop sto

// Run a complete program
let run (Prog topdecs) vs =
    let funEnv = initFunEnv topdecs
    let locEnv = ([], System.Int32.MaxValue)
    let sto = emptyStore
    callfun "main" [] locEnv funEnv sto



// Problem 5

(* ANSWER 5 HERE
    (i) This prints 10 because The value of p is changed when *(q - 1) = 10; is set, p and q are next to eachother. 
   (ii) This prints 0 because The of "*(&i + i) = 0;" is set to 0 after the while loop has finished doing its stuff, 
                              so the function will always print 0. That is because the while loop finds *a and sets its value to 0.  
*)

// void main() {
//   var p, q;
//   p = alloc(1);
//   q = alloc(1);
//   *(q - 1) = 10;
//   print(*p);
// }
let prog5i =
  Prog (
    [ ("main", [], ["p"; "q"], Block [
        Alloc (AccVar "p", Num 1);
        Alloc (AccVar "q", Num 1);
        Assign (AccDeref (Op ("-", Access (AccVar "q"), Num 1)), Num 10);
        Print (Access (AccDeref (Access (AccVar "p"))))
      ])
    ])

// void f() {
//   var i;
//   while (*(&i + i) != 1234) {
//     i = i + 1;
//   }
//   *(&i + i) = 0;
// }
// void main() {
//   var a, b, c, d;
//   a = 1234;
//   f();
//   print a;
// }
let prog5ii =
  Prog (
    [ ("f", [], ["i"], Block [
        While (Op ("!=", Access (AccDeref (Op ("+", Addr (AccVar "i"), Access (AccVar "i")))), Num 1234), Block [
          Assign (AccVar "i", Op ("+", Access (AccVar "i"), Num 1))
        ]);
        Assign (AccDeref (Op ("+", Addr (AccVar "i"), Access (AccVar "i"))), Num 0)
      ])
    ; ("main", [], ["a"; "b"; "c"; "d"], Block [
        Assign (AccVar "a", Num 1234);
        Call ("f", []);
        Print (Access (AccVar "a"))
      ])
    ])






