// T-501-FMAL Programming languages, Lectures  12-13
// Spring 2021

// Polymorphic type inference
// (based on code by Peter Sestoft)


module HigherFunInfer

open HigherFun


// A type is a type variable, Int, Bool or a function type.

type typ =
    | TVar of typevar                     // type variable           
    | Int
    | Bool
    | Fun of typ * typ                    // t -> t'

and typevar =
    (tvarkind * int) ref                  // kind and binding level  

and tvarkind =  
    | NoLink of string                    // uninstantiated type var
    | LinkTo of typ                       // type var instantiated to t


// A type scheme is a list of generalized type variables and a type.

type typescheme = 
    | TypeScheme of typevar list * typ    // forall t1,...,tn . t 


// Update type variable kind or level

let setTvKind (tv : typevar) (kind : tvarkind) : unit =
    let _, lvl = !tv
    tv := kind, lvl

let setTvLevel (tv : typevar) (lvl : int) : unit =
    let kind, _ = !tv
    tv := kind, lvl



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


let rec union xs ys = 
    match xs with 
    | []    -> ys
    | x::xs -> if List.contains x ys then union xs ys
               else x :: union xs ys

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
        let _, lvl = !tv
        setTvLevel tv (min lvl maxLevel)
    List.iter reducelevel tvs


let rec linkVarToType (tv : typevar) (t : typ) : unit =
    let _, lvl = !tv       // only calling it when _ is of form NoLink _ 
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
    match t1', t2' with
    | Int,  Int  -> ()
    | Bool, Bool -> ()
    | Fun (t11, t12), Fun (t21, t22) -> unify t11 t21; unify t12 t22
                                     // f ; g shorthand for  let () = f in g 
    | TVar tv1, TVar tv2 -> 
        let _, tv1level = !tv1
        let _, tv2level = !tv2
        if tv1 = tv2                then () 
        else if tv1level < tv2level then linkVarToType tv1 t2'
                                    else linkVarToType tv2 t1'
    | TVar tv1, _ -> linkVarToType tv1 t2'
    | _, TVar tv2 -> linkVarToType tv2 t1'
    | _, _ -> failwith ("cannot unify " + typeToString t1' +
                                                  " and " + typeToString t2')


// Generate fresh type variables 

let tyvarno : int ref = ref 0

let newTypeVar (lvl : int) : typevar = 
    let rec mkname i res = 
            if i < 26 then char(97+i) :: res
            else mkname (i/26-1) (char(97+i%26) :: res)
    let intToName i = new System.String(Array.ofList('\'' :: mkname i []))
    tyvarno := !tyvarno + 1;
    ref (NoLink (intToName (!tyvarno)), lvl)


// Generalize over type variables not free in the context; 
// i.e., over those whose level is higher than the current level

let rec generalize (lvl : int) (t : typ) : typescheme =
    let notfreeincontext tv = 
        let _, linkLvl = !tv 
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
                    | NoLink _ , _ -> t
                    | LinkTo t', _ -> copyType subst t'
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
        | NoLink name, _ -> name
        | _                -> failwith "we should not have ended up here"
    | Fun (t, t') -> "(" + showType t + " -> " + showType t' + ")"



// Type inference

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
        let () = unify tf (Fun (tx, tr))
        let env'' = (f, generalize lvl tf) :: env 
        infer ebody lvl env'' 

    | Num i -> Int

    | Plus (e1, e2) -> 
        let t1 = infer e1 lvl env 
        let t2 = infer e2 lvl env
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
    | True  -> Bool
    | False -> Bool
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


let inferTop e = 
    tyvarno := 0; showType (infer e 0 [])



// let f x = if x < 3 then true else 1011 in fact (f x) 




// let twice f = (let ff x = f (f x) in ff) in twice
let twice =
    LetFun ("twice", "f",
               LetFun ("ff", "x", Call (Var "f", Call (Var "f", Var "x")),
                   Var "ff"),
         Var "twice")


// let (|>) x = (let xto f = f x in xto) in (|>)
let pipe =
    LetFun ("|>", "x",
               LetFun ("xto", "f", Call (Var "f", Var "x"),
                   Var "xto"),
        Var "|>")           
                   
// let add1 x = x + 1
let add1 =
    LetFun ("add1", "x", Plus (Var "x", Num 1),
        Var "add1")


// let const x y = x

let k =
    LetFun ("const", "x",
                LetFun ("constx", "y", Var "x",
                    Var "constx"),
        Var "const")                    
                   

// let const x y = let z = y in x

let k' =
    LetFun ("const", "x",
                LetFun ("constx", "y", Let ("z", Var "y", Var "x"),
                    Var "constx"),
        Var "const")


// let const x y = let z = (y < 5) in x

let k'' =
    LetFun ("const", "x",
                LetFun ("constx", "y", Let ("z", Less (Var "y", Num 5), Var "x"),
                    Var "constx"),
        Var "const")

