// SC-T-501-FMAL Programming languages, Lecture 8
// Spring 2020

// Parsing
// (loosely based on code by Baris Aktemur)

module Parser

open Lexer

(*
type token =
    | NAME of string                      // variable names
    | LET | EQUAL | IN
    | INT of int                          // unsigned integers
    | PLUS | MINUS
    | TIMES
    | LPAR | RPAR                         // parentheses
    | ERROR of char                       // illegal symbols
*)

type expr =
    | Var of string
    | Let of string * expr * expr
    | CstI of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr

(*
We already looked at lexing, we now want to consider parsing:

         lexing                     parsing
string -----------> list of tokens -----------> abstr syntax tree
*)

(*
Let's first work with this concrete syntax for expressions:

e ---> 
    | x
    | let x = e in e
    | i
    | (e + e)
    | (e - e)
    | (e * e)

Note that parentheses are mandatory around operation applications
and that they cannot be used elsewhere.

This is not user-friendly, but makes parsing straightforward.
*) 


(*
The parser of expressions takes a list of tokens from the lexer.
It attempts to identify a prefix of the list as an expression
as it traverses and "consumes" it. The unconsumed part (suffix)
returned.
*)

let rec parseExpr1 (ts : token list) : expr * token list =

    match ts with
    | [] -> failwith "missing expression"
    
    | NAME x :: ts -> Var x, ts               // e ---> x
    
    | LET :: NAME x :: EQUAL :: ts ->         // e ---> let x = e in e
         let erhs, ts = parseExpr1 ts
         match ts with
         | IN :: ts ->
              let ebody, ts = parseExpr1 ts
              Let (x, erhs, ebody), ts
         | _ -> failwith "let without in"
    | LET :: NAME x :: _ -> failwith "let without equals sign"
    | LET :: _ -> failwith "lhs of def in let not a variable name"
    
    | INT i :: ts -> CstI i, ts               // e ---> i

    | LPAR :: ts ->                           // e ---> (e op e)
         let e1, ts = parseExpr1 ts
         match ts with
         | PLUS :: ts ->
              let e2, ts = parseExpr1 ts
              match ts with
              | RPAR :: ts -> Plus (e1, e2), ts
              | _ -> failwith "left paren without right paren"
         | MINUS :: ts ->
              let e2, ts = parseExpr1 ts
              match ts with
              | RPAR :: ts -> Minus (e1, e2), ts
              | _ -> failwith "left paren without right paren"
         | TIMES :: ts ->
              let e2, ts = parseExpr1 ts
              match ts with
              | RPAR :: ts -> Times (e1, e2), ts
              | _ -> failwith "left paren without right paren"
         | _  -> failwith "missing operation symbol"

    | IN :: _ -> failwith "in without let"
    | EQUAL :: _ -> failwith "equals sign without let"
    | PLUS :: _ | MINUS :: _ | TIMES :: _ ->
         failwith "operation symbol without left arg"
    | RPAR :: _ -> failwith "right paren without left paren"
    | ERROR _ :: _ -> failwith "illegal symbol"   
         

let parse1 (ts : token list) : expr =
    let (e, ts)  = parseExpr1 ts
    if ts = [] then e else failwithf "unconsumed tokens"


let lexParse1 (s : string) : expr = parse1 (lex s)


(*
What would happen if we worked with this grammar?

e --->
    | x
    | let x = e in e
    | i
    | e + e
    | e - e
    | e * e
    | (e)

Left-recursive rules like   e ---> e + e   yield a problem.
                            =      =
*)

(*
First, if literally we have the grammar as is, then "x + 3" is parsed as
Var x, with "+ 3" left over.

So we need the e + e case tried before the x case. 

But then, if we move the rule e ---> e + e to be the first, then our
our parser will recurse infinitely even on a string like just "x",
leading to stack overflow, since it will try

e ---> Plus (e1, e2)
  ---> Plus (Plus (e11, e12), e2)
  ---> Plus (Plus (Plus (e111, e112), e12), e2)
  ---> ...
*)



(*
The way around this is to introduce levels of expressions.

In our case, it suffices to work with these levels:

expressions
summands
factors

At the same time, we can assign operators their correct precedences.

+, -, * will become right-associative naturally.

For +, *, right-associativity is adequate.

For -, this is unfortunate; we really want left-associativity, since

x - (y - z) = (x - y) + z

We leave this problem for later.


e --->                            // expressions
    | s + e | s - e
    | s

s --->                            // summands
    | f * s
    | f

f --->                            // factors
    | x
    | let x = e in e
    | i
    | (e)

*)


(*
We now need to define three parsers simultaneously,
for expressions, summanads and factors.
*)


let rec parseExpr (ts : token list) : expr * token list =
                                  // e ---> s + e | s - e | s
     let (e1, ts) = parseSummand ts
     match ts with
     | PLUS :: ts ->
          let (e2, ts) = parseExpr ts
          Plus (e1, e2), ts
     | MINUS :: ts ->
          let (e2, ts) = parseExpr ts
          Minus (e1, e2), ts
     | _ -> e1, ts

and parseSummand (ts : token list) : expr * token list =
                                 // s ---> f * s | f  
     let e1, ts = parseFactor ts
     match ts with
     | TIMES :: ts ->
          let (e2, ts) = parseSummand ts
          Times (e1, e2), ts
     | _ -> e1, ts

and parseFactor (ts : token list) : expr * token list =
                                // f -> x | let x = e in e | i | (e)
     match ts with 
     | NAME x :: ts -> Var x, ts
     | LET :: NAME x :: EQUAL :: ts ->
         let (erhs, ts) = parseExpr ts
         match ts with
         | IN :: ts ->
              let ebody, ts = parseExpr ts
              Let (x, erhs, ebody), ts
         | _ -> failwith "let without in"
     | LET :: NAME x :: _ -> failwith "let without equals sign"
     | LET :: _ -> failwith "lhs of def in let not a variable name"
     | INT i :: ts -> CstI i, ts
     | LPAR :: ts ->
         let e, ts = parseExpr ts
         match ts with
         | RPAR :: ts -> e, ts
         | _ -> failwith "left paren without right paren"
     | _  -> failwith "not a factor"


let parse (ts : token list) : expr =
    let e, ts  = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"


let lexParse (s : string) : expr = parse (lex s)
