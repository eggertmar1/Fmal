// T-501-FMAL Programming languages, Practice class 6
// Spring 2021
// Solutions


// Problems 1-4


module FirstFunMultArgs

type expr =
    | Var of string                      // x
    | Let of string * expr * expr        // let x = erhs in ebody
    | Call of string * expr list
                        // f e1 e2 … en
    | LetFun of string * string list * expr * expr
                        // let f x1 x2 … xn = erhs in ebody
    | Num of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr
    | Neg of expr
    | True
    | False
    | Equal of expr * expr
    | Less of expr * expr
    | ITE of expr * expr * expr           // if e then e1 else e2


type token =
    | NAME of string                      // variable names
    | LET | EQUAL | IN
    | IF | THEN | ELSE | LESS
    | INT of int | TRUE | FALSE           // unsigned integers and bools
    | PLUS | MINUS | TIMES
    | LPAR | RPAR                         // parentheses
    | ERROR of char                       // illegal symbols

// Some helpers

let isDigit c = '0' <= c && c <= '9'

let digit2Int c = int c - int '0'

let isLowercaseLetter c = 'a' <= c && c <= 'z'

let isUppercaseLetter c = 'A' <= c && c <= 'Z'

let isLetter c = isLowercaseLetter c || isUppercaseLetter c

let word2Token (s : string) : token =
    match s with
    | "let"   -> LET
    | "in"    -> IN
    | "if"    -> IF
    | "then"  -> THEN
    | "else"  -> ELSE
    | "true"  -> TRUE
    | "false" -> FALSE
    | _       -> NAME s

// Lexer

let rec tokenize (cs : char list) : token list =
    match cs with
    | [] -> []
    | '+'::cs  -> PLUS :: tokenize cs
    | '-'::cs  -> MINUS :: tokenize cs
    | '*'::cs  -> TIMES :: tokenize cs
    | '='::cs  -> EQUAL :: tokenize cs
    | '<'::cs  -> LESS :: tokenize cs
    | ' '::cs  -> tokenize cs
    | '\t'::cs -> tokenize cs
    | '\n'::cs -> tokenize cs
    | '('::cs  -> LPAR :: tokenize cs
    | ')'::cs  -> RPAR :: tokenize cs
    | c::cs when isDigit c ->
        tokenizeInt cs (digit2Int c)
    | c::cs when isLowercaseLetter c ->
        tokenizeWord cs (string c)
    | c::cs -> ERROR c :: tokenize cs

and tokenizeInt cs (acc : int) =
    match cs with
    | c::cs when isDigit c ->
        tokenizeInt cs (acc * 10 + digit2Int c)
    | _ -> INT acc :: tokenize cs

and tokenizeWord cs (acc : string) =
    match cs with
    | c::cs when isLetter c || isDigit c ->
         tokenizeWord cs (acc + string c)
    | _ -> word2Token acc :: tokenize cs

let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)

let lex s = tokenize (string2Chars s)


// Parser

let rec parseExpr (ts : token list) : expr * token list =
                          // e ---> c = c | c < c | c
    let (e1, ts) = parseComparand ts
    match ts with
    | EQUAL :: ts ->
        let e2, ts = parseComparand ts
        Equal (e1, e2), ts
    | LESS :: ts ->
        let e2, ts = parseComparand ts
        Less (e1, e2), ts
    | _ -> e1, ts

and parseComparand (ts : token list) : expr * token list =
                          // c -> s ss
    let e, ts = parseSummand ts
    parseSummandList e ts

and parseSummandList (e1 : expr) (ts : token list) : expr * token list =
                          // ss ---> + s ss | - s ss | (empty)
    match ts with
    | PLUS :: ts ->
        let e2, ts = parseSummand ts
        parseSummandList (Plus (e1, e2)) ts
    | MINUS :: ts ->
        let e2, ts = parseSummand ts
        parseSummandList (Minus (e1, e2)) ts
    | _ -> e1, ts

and parseSummand (ts : token list) : expr * token list =
                          // s ---> f ff
    let e1, ts = parseFactor ts
    parseFactorList e1 ts

and parseFactorList (e1 : expr) (ts : token list) : expr * token list =
                          // ff ---> * s ss | (empty)
    match ts with
    | TIMES :: ts ->
        let e2, ts = parseFactor ts
        parseFactorList (Times (e1, e2)) ts
    | _ -> e1, ts
    
and parseFactor (ts : token list) : expr * token list =
                          // f ---> x aa | h
    match ts with
    | NAME x :: ts ->
        let eargs, ts = parseArgList ts
        let expr =
            if eargs = []
            then Var x           // No arguments, so just a variable
            else Call (x, eargs) // Some arguments, so a function call
        expr, ts
    | _ -> parseHead ts

and parseArgList (ts : token list) : expr list * token list =
                         // aa ---> a aa | (empty)
    match ts with
    | NAME _ :: _ | INT _ :: _ | TRUE :: _ | FALSE :: _ | LPAR _ :: _ ->
                                 // Here's a bit of lookahead
                                 // to avoid the need for backtracking
        let eatom, ts = parseArg ts
        let eatoms, ts = parseArgList ts
        eatom :: eatoms, ts
    | _ -> [], ts

and parseArg (ts : token list) : expr * token list =
                          // a ---> x | i | b | (e)
    match ts with
    | NAME x :: ts -> Var x, ts
    | INT i :: ts -> Num i, ts
    | TRUE  :: ts -> True, ts
    | FALSE :: ts -> False, ts
    | LPAR :: ts ->
        let e, ts = parseExpr ts
        match ts with
        | RPAR :: ts -> e, ts
        | _ -> failwith "left paren without right paren"
    | _  -> failwith "not a factor"

and parseHead (ts : token list) : expr * token list =
                          // h ---> a | let x xx = e in e | let x = e in e
                          //          | - f | if e then e else e
    match ts with
    | LET :: NAME x :: ts ->
        let names, ts = parseNameList ts
        match ts with
        | EQUAL :: ts ->
            let erhs, ts = parseExpr ts
            match ts with
            | IN :: ts ->
                let ebody, ts = parseExpr ts
                let expr =
                    if names = []
                    then // No arguments, so a Let
                      Let (x, erhs, ebody)
                    else // At least one argument, so a LetFun
                      LetFun (x, names, erhs, ebody)
                expr, ts
            | _ -> failwith "let without in"
        | _ -> failwith "let without equals sign"
    | LET :: _ -> failwith "1st token of lhs of def in let not a variable name"
    | MINUS :: ts ->
        let e, ts = parseFactor ts
        Neg e, ts
    | IF :: ts ->
        let e, ts = parseExpr ts
        match ts with
        | THEN :: ts ->
            let e1, ts = parseExpr ts
            match ts with
            | ELSE :: ts ->
                let e2, ts = parseExpr ts
                ITE (e, e1, e2), ts
            | _ -> failwith "if-then-else without else"
        | _ -> failwith "if-then-else without then"
    | _ -> parseArg ts

and parseNameList (ts : token list) : string list * token list =
                          // xx ---> x xx | (empty)
    match ts with
    | NAME x :: ts ->
        let xs, ts = parseNameList ts
        x :: xs, ts
    | _ -> [], ts


let parse (ts : token list) : expr =
    let (e, ts)  = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"

let lexParse (s : string) : expr = parse (lex s)


// Evaluation

type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env



type value =
    | I of int
    | B of bool
//(*  closures for static scope - include env from def
    | F of string list * expr * value envir
//*)
(* closures for dynamic scope - env not included
    | F of string list * expr
*)

let rec eval (e : expr) (env : value envir) : value =
    match e with
    | Var x ->
         match lookup x env with
         | I i -> I i
         | _   -> failwith "a function used as a value"
    | Let (x, erhs, ebody) ->
         let v = eval erhs env
         let env' = (x, v) :: env
         eval ebody env'
//(*    static scope version
    | Call (f, eargs) ->
         match lookup f env with
         | F (xs, ebody, env0) as clo ->
             let vs = List.map (fun earg -> eval earg env) eargs
             let xvs = List.zip xs vs
             let env' = xvs @ (f, clo) :: env0
             eval ebody env'
                             // function body evaluated
                             // in def-time environment
                             // + the value of the parameter
                             // + the value of the function name
         | _   -> failwith "variable called not a function"
    | LetFun (f, xs, erhs, ebody) ->
         let env' = (f, F (xs, erhs, env)) :: env
                             // def-time environment recorded in closure
         eval ebody env'
//*)
(*     dynamic scope version
    | Call (f, earg) ->
         match lookup f env with
         | F (x, ebody) as clo ->
             let vs = List.map (fun earg -> eval earg env) eargs
                             // argument evaluated in current env
             let xvs = List.zip xs vs
             let env' = xvs @ (f, clo) :: env
             eval ebody env'
                             // function body evaluated
                             // in current environment
                             // + the value of the parameter
                             // + the value of the function name
         | _   -> failwith "integer or boolean variable used called"
    | LetFun (f, xs, erhs, ebody) ->
         let env' = (f, F (xs, erhs)) :: env
                             // def-time envmnt not recorded in closure
         eval ebody env'
*)
    | Num i -> I i
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
    | True  -> B true
    | False -> B false
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


let evalString (e : string) (env : value envir) : value =
  eval (lexParse e) env

// evalString "let fact n = let factAcc acc n = if n < 1 then acc else factAcc (n * acc) (n - 1) in factAcc 1 n in fact 6" [];;

// evalString "let ack m n = if m = 0 then n + 1 else if n = 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1)) in ack 2 2" [];;

// evalString "let z = 0 in let f x y = if x = 0 then y + z else let z = z + 1 in f (x - 1) y in f 4 5" [];;
// (5 with static scoping, 9 with dynamic scoping)
