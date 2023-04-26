// 20.3.1
let vat n x = x + x * n/100

// 20.3.2
let unvat n x = (100 * x) / (100 + n)

// 20.3.3
let rec min f =
    let rec loop n =
        if f n = 0 then n
        else loop (n + 1) 
    loop 1 