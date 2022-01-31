// SC-T-501-FMAL Programming languages, Assignment 2 
// Spring 2020
// Solutions


module Assignment2

type token =
    | LBRACE | RBRACE | SEMICOLON | EQUAL
    | INT of int | NAME of string

let isDigit c = '0' <= c && c <= '9'
let digit2Int c = int c - int '0'
let isLowercaseLetter c = 'a' <= c && c <= 'z'
let isUppercaseLetter c = 'A' <= c && c <= 'Z'
let isLetter c = isLowercaseLetter c || isUppercaseLetter c


type expr = (string * contentsExpr) list

and contentsExpr =
    | CstI of int
    | Var of string
    | Record of expr 


type 'a envir = (string * 'a) list

let rec lookup (x : string) (env : 'a envir) : 'a =
    match env with
    | []          -> failwith (x + " not found")
    | (y, v)::env -> if x = y then v else lookup x env


type value = (string * contentsValue) list

and contentsValue =
    | I of int
    | R of value


type typ = (string * contentsType) list

and contentsType =
    | Int
    | Rec of typ
    

// Problem 1

let rec tokenize (cs : char list) : token list =
    match cs with
    | [] -> []
    | '='::cs  -> EQUAL :: tokenize cs
    | ';'::cs  -> SEMICOLON :: tokenize cs
    | ' '::cs  -> tokenize cs
    | '\t'::cs -> tokenize cs
    | '\n'::cs -> tokenize cs
    | '{'::cs  -> LBRACE :: tokenize cs
    | '}'::cs  -> RBRACE :: tokenize cs
    | c::cs when isDigit c ->
        tokenizeInt cs (digit2Int c)
    | c::cs when isLowercaseLetter c ->
        tokenizeWord cs (string c)
    | c::cs -> failwith (sprintf "Unexpected character '%c'" c)

and tokenizeInt cs (acc : int) =
    match cs with
    | c::cs when isDigit c ->
        tokenizeInt cs (acc * 10 + digit2Int c)
    | _ -> INT acc :: tokenize cs

and tokenizeWord cs (acc : string) =
    match cs with
    | c::cs when isLetter c || isDigit c ->
         tokenizeWord cs (acc + string c)
    | _ -> NAME acc :: tokenize cs

let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)

let lex s = tokenize (string2Chars s)


// Problem 2

let rec parseExpr (ts : token list) : expr * token list =
    match ts with
    | LBRACE :: ts ->
        let (fields, ts) = parseFieldList ts
        match ts with
        | RBRACE :: ts -> (fields, ts)
        | _ -> failwith "Expected '}'"
    | _ -> failwith "Expected '{'"

and parseFieldList (ts : token list) : expr * token list =
    match ts with
    | NAME x :: EQUAL :: ts ->
        let (c, ts) = parseContents ts
        match ts with
        | SEMICOLON :: ts ->
            let (fields, ts) = parseFieldList ts
            ((x, c) :: fields, ts)
        | _ -> ([(x, c)], ts)
    | NAME x :: ts -> failwith "Name without equals sign"
    | _ -> ([], ts)

and parseContents (ts : token list) : contentsExpr * token list =
    match ts with
    | INT i :: ts -> (CstI i, ts)
    | NAME x :: ts -> (Var x, ts)
    // SOLUTION: the following case has been added
    | LBRACE :: ts ->
        let (fields, ts) = parseFieldList ts
        match ts with
        | RBRACE :: ts -> (Record fields, ts)
        | _ -> failwith "Expected '}'"
    | _ :: ts -> failwith "Unexpected token"
    | [] -> failwith "Expected field contents"

let parse (ts : token list) : expr =
    let (e, ts)  = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"

let lexParse (s : string) : expr = parse (lex s)


// Problem 3

// (i)

(*

{x = {w = 1; z = 2}; y = x}
evaluates to {x = {w = 1; z = 2}; y = {w = 1; z = 2}}

{x = 20; y = {w = x}; z = {a = y}}
evaluates to {x = 20; y = {w = 20}; z = {a = {w = 20}}}

{x = 10; y = {x = 11; z = 12}; z = x}
evaluates to {x = 10; y = {x = 11; z = 12}; z = 10}

*)

// (ii)

let rec eval (e : expr) (env : contentsValue envir) : value =
    match e with
    | [] -> []
    | (x, c)::fields ->
        let v = evalContents c env
        // SOLUTION: the following line has changed
        (x, v) :: eval fields ((x, v)::env)
and evalContents (c : contentsExpr) (env : contentsValue envir) : contentsValue =
    match c with
    | CstI i -> I i
    | Var x -> lookup x env
    | Record fields -> R (eval fields env)

let evalString s = eval (lexParse s) []


// Problem 4

let rec infer (e : expr) (env : contentsType envir) : typ =
    match e with
    | [] -> []
    | (x, c)::fields ->
        let t = inferContents c env
        (x, t) :: infer fields ((x, t) :: env)

and inferContents (c : contentsExpr) (env : contentsType envir) : contentsType =
    match c with
    | CstI _ -> Int
    | Var x -> lookup x env
    | Record fields -> Rec (infer fields env)

let inferString s = infer (lexParse s) []


// Problem 5

let sum xs =
    let rec sumCont k xs =
        match xs with
        | [] -> k 0
        | x::xs ->
            if x < 0
            then k x
            else sumCont (fun s -> k (s + x)) xs
    sumCont (fun s -> s) xs



// Test cases for Problem 1:
// > lex "{}";;
// val it : token list = [LBRACE; RBRACE]
// > lex "{x = 1; y = 2}";;
// val it : token list =
//   [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2; RBRACE]
// > lex "{x = 1; y = 2; z = {}}";;
// val it : token list =
//   [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2;
//    SEMICOLON; NAME "z"; EQUAL; LBRACE; RBRACE; RBRACE]
// > lex "{x = 1; y = 2; z = {}; z2 = {x = y; z = x}}";;
// val it : token list =
//   [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2;
//    SEMICOLON; NAME "z"; EQUAL; LBRACE; RBRACE; SEMICOLON; NAME "z2"; EQUAL;
//    LBRACE; NAME "x"; EQUAL; NAME "y"; SEMICOLON; NAME "z"; EQUAL; NAME "x";
//    RBRACE; RBRACE]
// > lex "{x = {y = 1}; x = {x = x}}";;
// val it : token list =
//   [LBRACE; NAME "x"; EQUAL; LBRACE; NAME "y"; EQUAL; INT 1; RBRACE; SEMICOLON;
//    NAME "x"; EQUAL; LBRACE; NAME "x"; EQUAL; NAME "x"; RBRACE; RBRACE]


// Test cases for Problem 2:
// (These use `lexParse`, so require a solution to Problem 1.)
// > lexParse "{}";;
// val it : expr = []
// > lexParse "{x = 1; y = 2}";;
// val it : expr = [("x", CstI 1); ("y", CstI 2)]
// > lexParse "{x = 1; y = 2; z = {}}";;
// val it : expr = [("x", CstI 1); ("y", CstI 2); ("z", Record [])]
// > lexParse "{x = 1; y = 2; z = {}; z2 = {x = y; z = x}}";;
// val it : expr =
//   [("x", CstI 1); ("y", CstI 2); ("z", Record []);
//    ("z2", Record [("x", Var "y"); ("z", Var "x")])]
// > lexParse "{x = {y = 1}; x = {x = x}}";;
// val it : expr =
//   [("x", Record [("y", CstI 1)]); ("x", Record [("x", Var "x")])]

// The same test cases for Problem 2, without using `lexParse`:
// > parse [LBRACE; RBRACE];;
// val it : expr = []
// > parse [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2; RBRACE];;
// val it : expr = [("x", CstI 1); ("y", CstI 2)]
// > parse [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2; SEMICOLON; NAME "z"; EQUAL; LBRACE; RBRACE; RBRACE];;
// val it : expr = [("x", CstI 1); ("y", CstI 2); ("z", Record [])]
// > parse [LBRACE; NAME "x"; EQUAL; INT 1; SEMICOLON; NAME "y"; EQUAL; INT 2; SEMICOLON; NAME "z"; EQUAL; LBRACE; RBRACE; SEMICOLON; NAME "z2"; EQUAL; LBRACE; NAME "x"; EQUAL; NAME "y"; SEMICOLON; NAME "z"; EQUAL; NAME "x"; RBRACE; RBRACE];;
// val it : expr =
//   [("x", CstI 1); ("y", CstI 2); ("z", Record []);
//    ("z2", Record [("x", Var "y"); ("z", Var "x")])]
// > parse [LBRACE; NAME "x"; EQUAL; LBRACE; NAME "y"; EQUAL; INT 1; RBRACE; SEMICOLON; NAME "x"; EQUAL; LBRACE; NAME "x"; EQUAL; NAME "x"; RBRACE; RBRACE];;
// val it : expr =
//   [("x", Record [("y", CstI 1)]); ("x", Record [("x", Var "x")])]


// Test cases for Problem 3:
// (These require solutions to 1 and 2.)
// > evalString "{x = 1; y = x}";;
// val it : value = [("x", I 1); ("y", I 1)]
// > evalString "{x = 1; y = {x = 2}; z = x}";;
// val it : value =
//   [("x", I 1); ("y", R [("x", I 2)]); ("z", I 1)]
// > evalString "{x = {w = 1}; y = {z = x}}";;
// val it : value =
//   [("x", R [("w", I 1)]); ("y", R [("z", R [("w", I 1)])])]
// > evalString "{x = {y = 1}; x = {x = x}}";;
// val it : value =
//   [("x", R [("y", I 1)]); ("x", R [("x", R [("y", I 1)])])]
// > eval (lexParse "{b = {x = a; y = a}; c = {x = b; x = b}}") [("a", I 1)];;
// val it : value =
//   [("b", R [("x", I 1); ("y", I 1)]);
//    ("c",
//     R [("x", R [("x", I 1); ("y", I 1)]); ("x", R [("x", I 1); ("y", I 1)])])]
// > eval (lexParse "{y = {x = 2}; z = {w = x}}") [("x", I 1)];;
// val it : value =
//   [("y", R [("x", I 2)]); ("z", R [("w", I 1)])]

// Same test cases for Problem 3 (don't require solutions to 1 and 2):
// > eval [("x", CstI 1); ("y", Var "x")] [];;
// val it : value = [("x", I 1); ("y", I 1)]
// > eval [("x", CstI 1); ("y", Record [("x", CstI 2)]); ("z", Var "x")] [];;
// val it : value =
//   [("x", I 1); ("y", R [("x", I 2)]); ("z", I 1)]
// > eval [("x", Record [("w", CstI 1)]); ("y", Record [("z", Var "x")])] [];;
// val it : value =
//   [("x", R [("w", I 1)]); ("y", R [("z", R [("w", I 1)])])]
// > eval [("x", Record [("y", CstI 1)]); ("x", Record [("x", Var "x")])] [];;
// val it : value =
//   [("x", R [("y", I 1)]); ("x", R [("x", R [("y", I 1)])])]
// > eval [("b", Record [("x", Var "a"); ("y", Var "a")]); ("c", Record [("x", Var "b"); ("y", Var "b")])] [("a", I 1)];;
// val it : value =
//   [("b", R [("x", I 1); ("y", I 1)]);
//    ("c",
//     R [("x", R [("x", I 1); ("y", I 1)]); ("y", R [("x", I 1); ("y", I 1)])])]
// > eval [("y", Record [("x", CstI 2)]); ("z", Record [("w", Var "x")])] [("x", I 1)];;
// val it : value =
//   [("y", R [("x", I 2)]); ("z", R [("w", I 1)])]


// Test cases for Problem 4:
// (These require solutions to 1 and 2.)
// > inferString "{x = 1; y = x}";;
// val it : typ = [("x", Int); ("y", Int)]
// > inferString "{x = 1; y = {x = 2}; z = x}";;
// val it : typ =
//   [("x", Int); ("y", Rec [("x", Int)]); ("z", Int)]
// > inferString "{x = {w = 1}; y = {z = x}}";;
// val it : typ =
//   [("x", Rec [("w", Int)]); ("y", Rec [("z", Rec [("w", Int)])])]
// > inferString "{x = {y = 1}; x = {x = x}}";;
// val it : typ =
//   [("x", Rec [("y", Int)]); ("x", Rec [("x", Rec [("y", Int)])])]
// > infer (lexParse "{b = {x = a; y = a}; c = {x = b; x = b}}") [("a", Int)];;
// val it : typ =
//   [("b", Rec [("x", Int); ("y", Int)]);
//    ("c",
//     Rec
//       [("x", Rec [("x", Int); ("y", Int)]);
//        ("x", Rec [("x", Int); ("y", Int)])])]
// > infer (lexParse "{y = {x = 2}; z = {w = x}}") [("x", Int)];;
// val it : typ =
//   [("y", Rec [("x", Int)]); ("z", Rec [("w", Int)])]

// Same test cases for Problem 4 (don't require solutions to 1 and 2):
// > infer [("x", CstI 1); ("y", Var "x")] [];;
// val it : typ = [("x", Int); ("y", Int)]
// > infer [("x", CstI 1); ("y", Record [("x", CstI 2)]); ("z", Var "x")] [];;
// val it : typ =
//   [("x", Int); ("y", Rec [("x", Int)]); ("z", Int)]
// > infer [("x", Record [("w", CstI 1)]); ("y", Record [("z", Var "x")])] [];;
// val it : typ =
//   [("x", Rec [("w", Int)]); ("y", Rec [("z", Rec [("w", Int)])])]
// > infer [("x", Record [("y", CstI 1)]); ("x", Record [("x", Var "x")])] [];;
// val it : typ =
//   [("x", Rec [("y", Int)]); ("x", Rec [("x", Rec [("y", Int)])])]
// > infer [("b", Record [("x", Var "a"); ("y", Var "a")]); ("c", Record [("x", Var "b"); ("y", Var "b")])] [("a", Int)];;
// val it : typ =
//   [("b", Rec [("x", Int); ("y", Int)]);
//    ("c",
//     Rec
//       [("x", Rec [("x", Int); ("y", Int)]);
//        ("y", Rec [("x", Int); ("y", Int)])])]
// > infer [("y", Record [("x", CstI 2)]); ("z", Record [("w", Var "x")])] [("x", Int)];;
// val it : typ =
//   [("y", Rec [("x", Int)]); ("z", Rec [("w", Int)])]


// Test cases for Problem 5:
// > sum [1..5];;
// val it : int = 15
// > sum [-1];;
// val it : int = -1
// > sum [1;2;3;-1];;
// val it : int = 5
// > sum [1;2;3;-1;-2;3;-4];;
// val it : int = 5
// > sum [1;2;3;-1;2;3;4];;
// val it : int = 5

