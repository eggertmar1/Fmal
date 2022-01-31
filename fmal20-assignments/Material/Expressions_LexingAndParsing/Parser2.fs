// T-501-FMAL Programming languages, Lecture 8
// Spring 2021

// Parsing
// (loosely based on code by Baris Aktemur)


module Parser2

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
    | Num of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr

(*
Let's change our last parser. 

We want +, -, * to be left-associative, not right-associative.


We had in our grammar

e --->
    | s + e
    | s - e
    | s

s --->
    | f * s
    | f

We can reorganize the grammar as follows:

e --->                            // expressions
    | s ss 

ss --->                           // lists of summands
    | + s ss | - s ss
    | (empty)

s --->                            // summands
    | f ff

ff --->                           // lists of factors
    | * f ff
    | (empty)
    
f --->                            // factors
    | x
    | let x = e in e
    | i
    | (e)

A list of summands or factors can now be parsed by folding over it.
The first summand or factor provides the initial value of the accumulator.
*)



let rec parseExpr (ts : token list) : expr * token list =
                         // e ---> s ss
     let e1, ts = parseSummand ts
     parseSummandList e1 ts
     
     
and parseSummandList (e1 : expr) (ts : token list) : expr * token list =                           // ss ---> + s ss | - s ss | (empty)
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
          let (e2, ts) = parseFactor ts
          parseFactorList (Times (e1, e2)) ts
     | _ -> e1, ts
     
and parseFactor (ts : token list) : expr * token list =
                          // f ---> x | let x = e in e | i | (e) 
     match ts with 
     | NAME x :: ts -> Var x, ts
     | LET :: NAME x :: EQUAL :: ts ->
         let erhs, ts = parseExpr ts
         match ts with
         | IN :: ts ->
              let ebody, ts = parseExpr ts
              Let (x, erhs, ebody), ts
         | _ -> failwith "let without in"
     | LET :: NAME x :: _ -> failwith "let without equals sign"
     | LET :: _ -> failwith "lhs of def in let not a variable name"
     | INT i :: ts -> Num i, ts
     | LPAR :: ts ->
         let (e, ts) = parseExpr ts
         match ts with
         | RPAR :: ts -> e, ts
         | _ -> failwith "left paren without right paren"
     | _  -> failwith "not a factor"

let parse (ts : token list) : expr =
    let e, ts  = parseExpr ts
    if ts = [] then e else failwithf "unconsumed tokens"


let lexParse (s : string) : expr = parse (lex s)
