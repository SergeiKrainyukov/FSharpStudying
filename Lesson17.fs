// 17.1
let rec pow = function
 | (x,n) when n = 0 -> ""
 | (x,n) when n = 1 -> x
 | (x,n) -> x + pow(x, n - 1)
//17.2 
let rec isIthChar = function
    | (s, n, c) when n < 0 -> false
    | (s, n, c) when n > String.length s -> false
    | (s, n, c) when string(s).[n] = c -> true
    | _ -> false
//17.3
let rec occFromIth = function
    | (s, n, c) when n < 0 -> 0
    | (s, n, c) when String.length (s) = n || String.length (s) < n -> 0
    | (s, n, c) when (String.length (s) > n) && (s.[n] = c) -> 1 + occFromIth (s,n+1,c)
    | (s, n, c) when (String.length (s) > n) && (s.[n] <> c) -> occFromIth (s,n+1,c)
    | _ -> 0
