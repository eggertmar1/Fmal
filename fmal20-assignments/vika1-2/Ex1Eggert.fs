//1
let f n = n * 3 + 8
f 5 
//2
let g n = f (n-1)
g 12 
//3
let rec pow2 n = 
    match n with 
    | 0     -> 1 
    | _     -> 2 * pow2 (n-1)
pow2 7 
pow2 9 
//4 
let rec mc n = if n > 100 then n - 10 else mc (mc (n + 11))
mc 73
mc 84
//5
let rec groups3 = function 
    | []                -> []
    | [x]               -> [[x]]
    | [x1;x2]           -> [[x1;x2]]
    | x1::x2::x3::xs    -> [x1;x2;x3] :: groups3 xs 
// 6 
let rec takeDrop n xs =
    match n, xs with 
    | 0, xs      -> [], xs
    | n, []      -> [], []
    | n, x :: xs -> let t, d = takeDrop (n - 1) xs
                    x :: t, d

// not right almost. 
let rec groupsN n xs = 
    match n, xs with 
    | 0, xs     -> [], xs 
    | n, []     -> [], []
    | n , x :: xs -> let t, d = groupsN (n-1) xs
                     x :: t, d 
//correct soluton to 6
let rec groupsN n xs =
    match (n, xs) with 
    | n, [] -> [] 
    | n, xs -> let t, d = takeDrop n xs
               t :: groupsN n d