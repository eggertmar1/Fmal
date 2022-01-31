// SC-T-501-FMAL Programming languages, Assignment 3 
// Spring 2020
// Solutions


module Assignment3

// Problem 1

// fun1 : ('a -> 'b) -> 'a -> 'a * 'b
let fun1 f x = (x, f x)

// fun2 : ((’a -> ’b) -> (’a -> ’b)) -> ’a -> ’b
let rec fun2 f x = f (fun2 f) x


// Problem 2

// 'a -> 'b and int -> int
//   Unify with 'a := int and 'b := int

// 'a -> 'b -> 'c and 'd -> int
//   Do not unify (because 'b -> 'c does not unify with int)

// 'a -> 'a and ('b -> 'c) -> (int -> 'c)
//   Unify with 'a := int -> 'c and 'b := int

// 'a -> int and ('a -> 'a) -> int
//   Do not unify ('a cannot be unified with 'a -> 'a)


// Problem 3

// (\x. x) f
//   Normal form is f, type is A -> A

// (\g. \x. f (g x)) (\y. f y)
//   Normal form is \x. f (f x), type is A -> A

// \h. (\g. g (g f)) h
//   Normal form is \h. h (h f), type is ((A -> A) -> (A -> A)) -> (A -> A)

// ((\h. \k. k h) (\y. f (f y))) (\g. \x. g (g x))
//   Normal form is (\x. f (f (f (f x)))), type is A -> A

// ((\h. \f. f h) (\x. f (f x))) (\g. \x. g (g x))
//   Normal form is (\x. f (f (f (f x)))), type is A -> A


// Definitions for use in Problems 4 and 5

type styp =
    | SInt
    | SBool
    | SFun of styp * styp                       // t -> t'

type expr =
    | Var of string                             // x
    | Let of string * expr * expr               // let x = erhs in ebody
    | Call of expr * expr                       // efun earg
    | LetFun of string * string * expr * expr   // let f x = erhs in ebody
    | CstI of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Neg of expr
    | CstB of bool
    | Equal of expr * expr
    | Less of expr * expr
    | ITE of expr * expr * expr
    | Annot of expr * styp                      // (x : t)


type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env

type value =
    | I of int
    | B of bool
    | F of string * string * expr * value envir
                          // (fun f -> fun x -> e), [x1,v1; ...; xn,vn]


// Problem 4

let rec freevars bound = function
  | Var x -> if List.contains x bound then [] else [x]
  | Let (x, erhs, ebody) ->
      freevars bound erhs @ freevars (x :: bound) ebody
  | LetFun (f, x, erhs, ebody) ->
      freevars (f :: x :: bound) erhs
      @ freevars (f :: bound) ebody
  | CstI _ | CstB _ -> []
  | Neg e -> freevars bound e
  | Call (e1, e2) | Plus (e1, e2) | Minus (e1, e2)
  | Times (e1, e2) | Equal (e1, e2) | Less (e1, e2) ->
      freevars bound e1 @ freevars bound e2
  | ITE (e, e1, e2) ->
      freevars bound e
      @ freevars bound e1
      @ freevars bound e2
  | Annot (e, _) -> freevars bound e


let restrictEnv (bound : string list) (e : expr) (env : value envir) : value envir =
  let fv = freevars bound e
  List.filter (fun (x, _) -> List.contains x fv) env


let rec eval (e : expr) (env : value envir) : value =
    match e with
    | Var x  ->  lookup x env
    | Let (x, erhs, ebody) ->
         let v = eval erhs env
         let env' = (x, v) :: env
         eval ebody env'
    | Call (efun, earg) ->
         let clo = eval efun env
         match clo with
         | F (f, x, ebody, env0) as clo ->
             let v = eval earg env
             let env' = (x, v) :: (f, clo) :: env0
             eval ebody env'
         | _   -> failwith "expression called not a function"
    | LetFun (f, x, erhs, ebody) ->
         // SOLUTION: This is the critical case.
         let env' = (f, F (f, x, erhs, (restrictEnv [f; x] erhs env))) :: env
         eval ebody env'
    | CstI i -> I i
    | Plus  (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 + i2)
         | _ -> failwith "argument of + not integers"
    | Minus  (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 - i2)
         | _ -> failwith "arguments of - not integers"
    | Times (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> I (i1 * i2)
         | _ -> failwith "arguments of * not integers"
    | Neg e ->
         match eval e env with
         | I i -> I (- i)
         | _ -> failwith "argument of negation not an integer"
    | CstB b -> B b
    | Equal (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> B (i1 = i2)
         | _ -> failwith "arguments of = not integers"
    | Less (e1, e2) ->
         match (eval e1 env, eval e2 env) with
         | I i1, I i2 -> B (i1 < i2)
         | _ -> failwith "arguments of < not integers"
    | ITE (e, e1, e2) ->
         match eval e env with
         | B b -> if b then eval e1 env else eval e2 env
         | _ -> failwith "guard of if-then-else not a boolean"
    | Annot (e, t) -> eval e env


// Definitions for use in Problem 5

let rec union xs ys =
    match xs with
    | []    -> ys
    | x::xs -> if List.contains x ys then union xs ys
               else x :: union xs ys


// A type is a type variable, Int, Bool or a function type.

type typ =
     | TVar of typevar                     // type variable
     | Int
     | Bool
     | Fun of typ * typ                    // t -> t'

and tvarkind =
     | NoLink of string                    // uninstantiated tvar tv
     | LinkTo of typ                       // tv instantiated to t

and typevar =
     (tvarkind * int) ref                  // kind and binding level


// A type scheme is a list of generalized type variables and a type.

type typescheme =
     | TypeScheme of typevar list * typ    // forall t1,...,tn . t


// Update type variable kind or level

let setTvKind (tv : typevar) (kind : tvarkind) =
    let (_, lvl) = !tv
    tv := (kind, lvl)

let setTvLevel (tv : typevar) (lvl : int) =
    let (kind, _) = !tv
    tv := (kind, lvl)


// Normalize a type; make type variable point directly to the
// associated type (if any).

let rec normType (t : typ) : typ =
    match t with
    | TVar tv ->
        match !tv with
        | LinkTo t', _ -> let tn = normType t'
                          setTvKind tv (LinkTo tn); tn
        | _ -> t
    |  _ -> t


// Make type variable tv equal to type t by linking it to t,
// but first check that tv does not occur in t, and reduce the level
// of all type variables in t to that of tyvar.

// This is the `union' operation in the union-find algorithm.

let rec freeTypeVars (t : typ) : typevar list =
    match normType t with
    | TVar tv      -> [tv]
    | Int          -> []
    | Bool         -> []
    | Fun (t1, t2) -> union (freeTypeVars t1) (freeTypeVars t2)

let occursCheck (tv : typevar) (tvs : typevar list) : unit =
    if List.contains tv tvs then failwith "type error: circularity"
    else ()

let pruneLevel (maxLevel : int) (tvs : typevar list) : unit =
    let reducelevel tv =
        let (_, lvl) = !tv
        setTvLevel tv (min lvl maxLevel)
    List.iter reducelevel tvs

let rec linkVarToType (tv : typevar) (t : typ) : unit =
    let (_, lvl) = !tv
    let tvs = freeTypeVars t
    occursCheck tv tvs;
    pruneLevel lvl tvs;
    setTvKind tv (LinkTo t)


// Unify two types, equating type variables with types as necessary

let rec typeToString (t : typ) : string =
    match t with
    | TVar  _      -> failwith "we should not have ended up here"
    | Int          -> "Int"
    | Bool         -> "Bool"
    | Fun (t1, t2) -> "function"

let rec unify (t1 : typ) (t2 : typ) : unit =
    let t1' = normType t1
    let t2' = normType t2
    match (t1', t2') with
    | (Int,  Int)  -> ()
    | (Bool, Bool) -> ()
    | (Fun (t11, t12), Fun (t21, t22)) -> unify t11 t21; unify t12 t22
    | (TVar tv1, TVar tv2) ->
      let (_, tv1level) = !tv1
      let (_, tv2level) = !tv2
      if tv1 = tv2                then ()
      else if tv1level < tv2level then linkVarToType tv1 t2'
                                  else linkVarToType tv2 t1'
    | (TVar tv1, _       ) -> linkVarToType tv1 t2'
    | (_,        TVar tv2) -> linkVarToType tv2 t1'
    | (Int,      t) -> failwith ("cannot unify Int and " + typeToString t)
    | (Bool,     t) -> failwith ("cannot unify Bool and " + typeToString t)
    | (Fun _,    t) -> failwith ("cannot unify function and " + typeToString t)



// Generate fresh type variables

let tyvarno : int ref = ref 0

let newTypeVar (lvl : int) : typevar =
    let rec mkname i res =
            if i < 26 then char(97+i) :: res
            else mkname (i/26-1) (char(97+i%26) :: res)
    let intToName i = new System.String(Array.ofList('\'' :: mkname i []))
    tyvarno := !tyvarno + 1;
    ref (NoLink (intToName (!tyvarno)), lvl)


// Generalize over type variables not free in the context; that is,
// over those whose level is higher than the current level

let rec generalize (lvl : int) (t : typ) : typescheme =
    let notfreeincontext tv =
        let (_, linkLvl) = !tv
        linkLvl > lvl
    let tvs = List.filter notfreeincontext (freeTypeVars t)
    TypeScheme (tvs, t)


// Copy a type, replacing bound type variables as dictated by subst
// and non-bound ones by a copy of the type linked to.

let rec copyType (subst : (typevar * typ) list) (t : typ) : typ =
    match t with
    | TVar tv ->
      let rec loop subst =
          match subst with
               | (tv', t') :: subst -> if tv = tv' then t' else loop subst
               | [] -> match !tv with
                       | (NoLink _, _)  -> t
                       | (LinkTo t', _) -> copyType subst t'
      loop subst
    | Fun (t1,t2) -> Fun (copyType subst t1, copyType subst t2)
    | Int         -> Int
    | Bool        -> Bool


// Create a type from a type scheme (tvs, t) by instantiating all the
// type scheme's parameters tvs with fresh type variables

let specialize (lvl : int) (TypeScheme (tvs, t)) : typ =
    let bindfresh tv = (tv, TVar (newTypeVar lvl))
    match tvs with
    | [] -> t
    | _  -> let subst = List.map bindfresh tvs
            copyType subst t


// Pretty-print type, using names 'a, 'b, ... for type variables

let rec showType (t : typ) : string =
        match normType t with
        | Int          -> "Int"
        | Bool         -> "Bool"
        | TVar tv      ->
            match !tv with
            | (NoLink name, _) -> name
            | _                -> failwith "we should not have ended up here"
        | Fun (t, t') -> "(" + showType t + " -> " + showType t' + ")"


let rec stypToTyp = function
  | SInt -> Int
  | SBool -> Bool
  | SFun (t1, t2) -> Fun (stypToTyp t1, stypToTyp t2)


// Problem 5

let rec infer (e : expr) (lvl : int) (env : typescheme envir) : typ =
    match e with
    | Var x  -> specialize lvl (lookup x env)
    | Let (x, erhs, ebody) ->
      let lvl' = lvl + 1
      let tx = infer erhs lvl' env
      let env' = (x, generalize lvl tx) :: env
      infer ebody lvl env'
    | Call (efun, earg) ->
      let tf = infer efun lvl env
      let tx = infer earg lvl env
      let tr = TVar (newTypeVar lvl)
      unify tf (Fun (tx, tr)); tr
    | LetFun (f, x, erhs, ebody) ->
      let lvl' = lvl + 1
      let tf = TVar (newTypeVar lvl')
      let tx = TVar (newTypeVar lvl')
      let env' = (x, TypeScheme ([], tx))
                      :: (f, TypeScheme ([], tf)) :: env
      let tr = infer erhs lvl' env'
      let _    = unify tf (Fun (tx, tr))
      let env'' = (f, generalize lvl tf) :: env
      infer ebody lvl env''
    | CstI i -> Int
    | Plus (e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e1 lvl env
         unify Int t1; unify Int t2; Int
    | Minus (e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e2 lvl env
         unify Int t1; unify Int t2; Int
    | Times (e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e2 lvl env
         unify Int t1; unify Int t2; Int
    | Neg e ->
         let t = infer e lvl env
         unify Int t; Int
    | CstB b -> Bool
    | Equal (e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e2 lvl env
         unify Int t1; unify Int t2; Bool
    | Less (e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e2 lvl env
         unify Int t1; unify Int t2; Bool
    | ITE (e, e1, e2) ->
         let t1 = infer e1 lvl env
         let t2 = infer e2 lvl env
         unify Bool (infer e lvl env); unify t1 t2; t1
    | Annot (e, t) ->
         // SOLUTION: This is the critical case.
         let t' = infer e lvl env
         unify (stypToTyp t) t';
         t'

let inferTop e =
    tyvarno := 0; showType (infer e 0 [])

// Test cases for Problem 4:
// > eval (LetFun ("f", "x", Var "x", Var "f")) [];;
// val it : value = F ("f","x",Var "x",[])
// > eval (Let ("x", CstI 0, LetFun("f", "y", Call (Var "f", Var "y"), Var "f"))) [];;
// val it : value = F ("f","y",Call (Var "f",Var "y"),[])
// > eval (Let ("x", CstI 0, LetFun("f", "y", Call (Var "f", Var "x"), Var "f"))) [];;
// val it : value = F ("f","y",Call (Var "f",Var "x"),[("x", I 0)])
// > eval (Let ("x", CstI 0, LetFun("f", "x", Call (Var "f", Var "x"), Var "f"))) [];;
// val it : value = F ("f","x",Call (Var "f",Var "x"),[])
// > eval (LetFun ("f", "x", ITE (Less (Var "x", CstI 1), CstI 1, Times (Var "x", Call (Var "f", Minus (Var "x", CstI 1)))), Let ("y", Call (Var "f", CstI 4), LetFun("g", "x", Plus (Var "x", Var "y"), Var "g")))) [];;
// val it : value = F ("g","x",Plus (Var "x",Var "y"),[("y", I 24)])
// > eval (LetFun ("g", "x", LetFun ("f", "y", Plus (Var "x", Var "y"), Var "f"), Let ("x", CstI 1, Call (Var "g", Call (Call (Var "g", CstI 2), Var "x"))))) [];;
// val it : value = F ("f","y",Plus (Var "x",Var "y"),[("x", I 3)])

// Test cases for Problem 5:
// > inferTop (LetFun ("g", "x", Var "x", Var "g"));;
// val it : string = "('c -> 'c)"
// > inferTop (LetFun ("g", "x", Annot (Var "x", SInt), Var "g"));;
// val it : string = "(Int -> Int)"
// > inferTop (LetFun ("g", "x", Annot (Var "x", SInt), Call (Var "g", CstB true)));;
// System.Exception: cannot unify Int and Bool
// > inferTop (LetFun ("g", "x", Call (Var "g", Annot (Var "x", SInt)), Var "g"));;
// val it : string = "(Int -> 'd)"
// > inferTop (LetFun ("g", "x", Call (Var "g", Annot (Var "x", SInt)), Var "g"));;
// val it : string = "(Int -> 'd)"
// > inferTop (LetFun ("g", "f", Call (Annot (Var "f", SFun (SInt, SBool)), CstI 0), Var "g"));;
// val it : string = "((Int -> Bool) -> Bool)"
// > inferTop (LetFun ("g", "f", Call (Annot (Var "f", SFun (SInt, SBool)), CstI 0), Call (Var "g", LetFun ("f", "x", Var "x", Var "f"))));;
// > System.Exception: cannot unify Bool and Int

