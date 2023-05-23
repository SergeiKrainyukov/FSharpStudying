let f n =
    let mutable result = 1
    for i in 2 .. n do
        result <- result * i
    result

let fibo n =
    let mutable a = 0
    let mutable b = 1
    for i in 2 .. n do
        let temp = a
        a <- b
        b <- temp + b
    if n = 0 then a else b
