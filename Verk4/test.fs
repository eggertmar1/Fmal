let drop23 xs = 
    match xs with 
    | [] -> []
    | [x1] -> [x1]
    | [x1;x2] -> [x1]
    | x1::x2::x3::xs -> x1::xs
    
let sqMul5 xs = List.map( fun x -> x * x) (List.filter (fun x -> x % 5 = 0) xs)  // modulo: %


let list1 = [1;2;3;4;5;6;7;8;9;10]

let rec safeTake n xs = 
    match n, xs with 
    | 0,_ -> Some []
    | _, [] -> None 
    | n, x::xs -> match safeTake (n-1) xs with
                    | None -> None 
                    | Some ys -> Some (x :: ys)

    



let rec groups3 = function
    | []                   -> []
    | [x1]                 -> [[x1]]
    | [x1; x2]             -> [[x1; x2]]
    | x1 :: x2 :: x3 :: xs -> [x1; x2; x3] :: groups3 xs


let rec group2 = function 
    | []              -> []
    | [x1]            -> []
    | x1::x2::xs      -> [x1;x2] :: group3 xs



let rec group3 = function 
    | []            -> []
    | [x1]          -> []
    | [x1;x2]       -> []
    | x1::x2::x3::xs -> [x1;x2;x3] :: group2 xs 


let group23 xs = (group2 xs) 





(*

(6 p) Using the library functions List.filter and List.map, write an
   F# function

   div57 : int list -> int list

   that takes a list of integers, keeps those that divide by 5 or 7 (or
   both), and then adds to each of the kept elements.

   > div57 [1; 3; 5; 7; 8; 10; 14; 15; 21; 26; 30];;
   val it : int list = [6; 8; 11; 15; 16; 22; 31]


OLDEXAM: Using List.filter and List.map, write a function sqMul5 : int list -> int list that
    returns the squares of those elements of a given list that are multiples of 5.

*)

let sqMul5 xs = List.map (fun x -> x * x) (List.filter (fun x -> x mod 5 = 0) xs)




let div57 xs = List.map (fun x -> x + 1) (List.filter (fun x -> x % 5 = 0 || x % 7 = 0) xs)






(*
    (5 p) Write an F# function

   safeFind : ('a -> bool) -> 'a list -> 'a option
   
   that returns the first element of given list satisfying the given
   predicate.  It should signal failure (by returning None) if there
   is no such element.

   > safeFind (fun x -> x < 0) [16; 20; -3; 5; 4; -8; 2];;
   val it : (int list) option = Some (-3)

   > safeFind (fun x -> x mod 2 = 0) [5; 7; 1; 13];;
   val it : (int list) option = None


OLDEXAM: 
    (6 p) Using recursion and pattern-matching on the list (and in a relevant case also on the result of a
    recursive call), write a function safeTake : int -> 'a list -> ('a list) option that returns
    the given number of rst elements of a given list if the number is non-negative and the list has
    many enough elements. The function should signal failure if the number is negative or the list is
    too short.
    > safeTake 3 [16; 20; 5; 4; 2];;
    val it : (int list) option = Some [16; 20; 5]
    > safeTake 2 [5];;
    val it : (int list) option = None

*)


let rec safeTake n xs =
    match n, xs with
    | 0, _ -> Some []
    | _, [] -> None
    | n, x::xs -> match safeTake (n-1) xs with
                    | None -> None
                    | Some ys -> Some (x :: ys)



let rec safeFind expr xs = 
    match  xs with 
    | []          -> None
    | x1::xs     -> List.filter(expr xs)
                    | true -> Some x1
                    | false -> safeFind expr xs 







let rec safeFind f xs =
    match xs with
    | [] -> None
    | x::xs -> match f x with
                | true -> Some x
                | false -> safeFind f xs
