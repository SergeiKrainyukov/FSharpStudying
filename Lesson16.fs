// 16.1
let notDivisible (n, m) = m % n = 0

let rec isPrime n i =
    if i >= n then
        true
    else if n % i = 0 then
        false
    else
        isPrime n (i+1)

// 16.2
let prime n =
    if n <= 1 then
        false
    elif n = 2 then
        true
    else
        isPrime n 2