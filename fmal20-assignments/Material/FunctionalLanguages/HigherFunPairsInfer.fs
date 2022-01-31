// T-501-FMAL Programming languages, Practice class 8
// Spring 2020

// Problem 3, higher-order functions with pairs:
// polymorphic type inference


module HigherFunPairsInfer

open HigherFunPairs


// Types

type typ =
     | TVar of typevar                     // type variable           
     | Int
     | Bool
     | Fun of typ * typ                    // t -> t'

     | Pair of typ * typ                   // t * t'      NEW!

and typevar =
     (tvarkind * int) ref                  // kind and binding level

and tvarkind =  
     | NoLink of string                    // uninstantiated tvar tv
     | LinkTo of typ                       // tv instantiated to t


// Type schemes

type typescheme = 
     | TypeScheme of typevar list * typ    // forall t1,...,tn . t


let setTvKind (tv : typevar) (kind : tvarkind) =
    let (_, lvl) = !tv
    tv := (kind, lvl)

let setTvLevel (tv : typevar) (lvl : int) =
    let (kind, _) = !tv
    tv := (kind, lvl)


let rec normType (t : typ) : typ = 
    match t with
    | TVar tv ->
        match !tv with 
        | LinkTo t', _ -> let tn = normType t' 
                          setTvKind tv (LinkTo tn); tn
        | _ -> t
    |  _ -> t


let rec union xs ys = 
    match xs with 
    | []    -> ys
    | x::xs -> if List.contains x ys then union xs ys
               else x :: union xs ys

let rec freeTypeVars (t : typ) : typevar list = 
    match normType t with
    | TVar tv       -> [tv]    
    | Int           -> []
    | Bool          -> []
    | Fun (t1, t2)  -> union (freeTypeVars t1) (freeTypeVars t2)
    
    | Pair (t1, t2) -> union (freeTypeVars t1) (freeTypeVars t2)
                                                           // NEW!
    

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


// Unification of types

let rec typeToString (t : typ) : string =
    match t with
    | TVar  _       -> failwith "we should not have ended up here"    
    | Int           -> "Int"
    | Bool          -> "Bool"
    | Fun (t1, t2)  -> "a function type"
 
    | Pair (t1, t2) -> "a pair type"                      // NEW!

let rec unify (t1 : typ) (t2 : typ) : unit =
    let t1' = normType t1
    let t2' = normType t2
    match t1', t2' with
    | Int,  Int  -> ()
    | Bool, Bool -> ()
    | Fun (t11, t12), Fun (t21, t22) ->
         unify t11 t21; unify t12 t22
    | Pair (t11, t12), Pair (t21, t22) ->                 // NEW!
         unify t11 t21; unify t12 t22         
    | TVar tv1, TVar tv2 -> 
        let _, tv1level = !tv1
        let _, tv2level = !tv2
        if tv1 = tv2                then () 
        else if tv1level < tv2level then linkVarToType tv1 t2'
                                    else linkVarToType tv2 t1'
    | TVar tv1, _ -> linkVarToType tv1 t2'
    | _, TVar tv2 -> linkVarToType tv2 t1'
    | _, _ -> failwith ("cannot unify" + typeToString t1' +
                                       " and " + typeToString t2')


let tyvarno : int ref = ref 0

let newTypeVar (lvl : int) : typevar = 
    let rec mkname i res = 
            if i < 26 then char(97+i) :: res
            else mkname (i/26-1) (char(97+i%26) :: res)
    let intToName i = new System.String(Array.ofList('\'' :: mkname i []))
    tyvarno := !tyvarno + 1;
    ref (NoLink (intToName (!tyvarno)), lvl)


let rec generalize (lvl : int) (t : typ) : typescheme =
    let notfreeincontext tv = 
        let (_, linkLvl) = !tv 
        linkLvl > lvl
    let tvs = List.filter notfreeincontext (freeTypeVars t)
    TypeScheme (tvs, t) 


let rec copyType (subst : (typevar * typ) list) (t : typ) : typ = 
    match t with
    | TVar tv ->
        let rec loop subst =          
            match subst with 
            | (tv', t') :: subst -> if tv = tv' then t' else loop subst
            | [] -> match !tv with
                    | NoLink _, _  -> t
                    | LinkTo t', _ -> copyType subst t'
        loop subst
    | Int           -> Int
    | Bool          -> Bool      
    | Fun (t1, t2)  -> Fun (copyType subst t1, copyType subst t2)

    | Pair (t1, t2) -> Pair (copyType subst t1, copyType subst t2)
                                                            // NEW!


let specialize (lvl : int) (TypeScheme (tvs, t)) : typ =
    let bindfresh tv = (tv, TVar (newTypeVar lvl))
    match tvs with
    | [] -> t
    | _  -> let subst = List.map bindfresh tvs
            copyType subst t

let rec showType (t : typ) : string =
    match normType t with
    | Int          -> "Int"
    | Bool         -> "Bool"
    | TVar tv      -> 
        match !tv with
        | NoLink name, _ -> name
        | _              -> failwith "we should not have ended up here"
    | Fun (t, t')  -> "(" + showType t + " -> " + showType t' + ")"

    | Pair (t, t') -> "(" + showType t + " * " + showType t' + ")"
                                                           // NEW!


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

    | MkPair (e1, e2) ->                                  // NEW!
         Pair (infer e1 lvl env, infer e2 lvl env)
    | Fst e ->                                            // NEW!
         let tp = infer e lvl env 
         let t1 = TVar (newTypeVar lvl)
         let t2 = TVar (newTypeVar lvl)
         unify tp (Pair (t1, t2)); t1
    | Snd e ->                                            // NEW!
         let tp = infer e lvl env 
         let t1 = TVar (newTypeVar lvl)
         let t2 = TVar (newTypeVar lvl)
         unify tp (Pair (t1, t2)); t2
         
 
let inferTop e = 
    tyvarno := 0; showType (infer e 0 [])


