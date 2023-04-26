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