// T-501-FMAL Programming languages, Lecture 7
// Spring 2021

// Lexing
// (based on code by Baris Aktemur)


module Lexer

(*
Our interpreters and compilers worked on abstract syntax trees.

How to get from concrete syntax strings to abstract syntax trees?

                      prettyprinting
abstract syntax tree ---------------> string

           lexing                     parsing
	(tokenizing)   
string   ---------->  list of tokens ---------> abstract syntax tree

Let's work this out for the expressions language.
*)


(*
Remember this was the expressions language in abstract syntax
(abstract syntax trees).
*)

type expr =
    | Var of string
    | Let of string * expr * expr
    | Num of int
    | Plus of expr * expr
    | Minus of expr * expr
    | Times of expr * expr


// Tokens we want to collect from a string

type token =
    | NAME of string                      // variable names
    | LET | EQUAL | IN
    | INT of int                          // unsigned integers
    | PLUS | MINUS | TIMES
    | LPAR | RPAR                         // parentheses
    | ERROR of char                       // illegal symbols 


// Some helpers to classify and convert characters

let isDigit c = '0' <= c && c <= '9'

let digit2Int (c : char) = int c - int '0'

let isLowercaseLetter c = 'a' <= c && c <= 'z'

let isUppercaseLetter c = 'A' <= c && c <= 'Z'

let isLetter c = isLowercaseLetter c || isUppercaseLetter c


// Words to tokens

(*
Let's call a string starting with a small letter
and consisting only of letters and numbers a word.

A word is a name unless it's one of the keywords
"let" or "in".
*)

let word2Token (s : string) : token =
    match s with
    | "let" -> LET
    | "in"  -> IN
    | _     -> NAME s


// Lexer

(*
We define three functions by simultaneous recursion. 

They implement a little state machine
traversing the given string.

Wherever we are in the top-level string,
this machine is in one of three modes or states:

- the "default" mode  (tokenize)
- in the middle of lexing an unsigned integer (tokenizeInt)
- in the middle of lexing a word (tokenizeWord)

In the latter two modes, the machine also accumulates
some information. 

Based on the character inspected, the machine switches
mode and goes to the next character.
*)

let rec tokenize (cs : char list) : token list =
    match cs with
    | [] -> []
    | '+'::cs  -> PLUS :: tokenize cs
    | '-'::cs  -> MINUS :: tokenize cs  
    | '*'::cs  -> TIMES :: tokenize cs
    | '='::cs  -> EQUAL :: tokenize cs
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


(*
Note the combination of patterns with "when" in the code above.
This allows one to save on the number of different cases
by essentially mixing pattern-matching and if-then-else. 
*)


let string2Chars (s : string) : char list =
    let rec helper cs i =
        if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
    helper [] (String.length s)     

let lex s = tokenize (string2Chars s)

