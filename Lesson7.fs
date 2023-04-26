// 7.1.1
let rec fibo = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibo (n-1) + fibo (n-2)

// 7.1.2
let rec sum = function
    | 1 -> 1
    | n -> n + sum (n-1)
// printfn "%d" (sum 3) 

// 7.1.3
let rec sum2 = function 
 | (m,0) -> m 
 | (m,n) -> m + n + sum2 (m, n-1)

printfn "%d" (sum2 (2,2))

// 20.3.1
let vat n x = (float n / 100.0 + 1.0) * x

// 20.3.2
let unvat n y = y / (float n / 100.0 + 1.0)

// 20.3.3
let rec min f =
    let rec loop n =
        if f n = 0 then n
        else loop (n + 1) 
    loop 1 