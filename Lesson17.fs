// 17.1
let rec pow = function 
 | (x,1) -> x
 | (x,n) -> x + pow(x, n - 1)
//17.2 
let rec isIthChar (s, n, c) = string(s).[n] = c
//17.3
let rec occFromIth = function
    | (s, n, c) when String.length (s) = n || String.length (s) < n -> 0
    | (s, n, c) when (String.length (s) <> n) && (s.[n] = c) -> 1 + occFromIth (s,n+1,c)
    | (s, n, c) when (String.length (s) <> n) && (s.[n] <> c) -> occFromIth (s,n+1,c)