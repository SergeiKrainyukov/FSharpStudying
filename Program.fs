﻿module SomeNamespace.SomeModule'

let toCoppers = function
  | (g,s,c) -> c + s*12 + 240*g

let normalize = function
 | x when x<240 -> (0, x/12, x - 12*(x/12))
 | x when x>=240 -> (x/240, (x%240)/12, (x%240)%12)
 |_->(0,0,0)

// 23.4.1
let (.+.) x y = normalize (toCoppers x + toCoppers y)
let (.-.) x y = normalize (toCoppers x - toCoppers y)
// let (.-.) x y = ...

printfn "%A" ((1,5,0).-.(0,10,0))

printfn "%d" (508%240)


// 23.4.2
let (.+) (a, b) (c, d) = (a + c, b + d)
let (.-) (a, b) (c, d) = (a - c, b - d)
let (.*) (a, b) (c, d) = (a * c - b * d, b * c + a * d)
// let (./) (a, b) (c, d) = (a,b) .* inverseForDivide (c,d)
// let (./) (a, b) (c, d) = let r = c * c + d * d in (a * c + b * d) / r, (b * c - a * d) / r
let (./) (a, b) (c, d) = ( (a*c+b*d)/(c*c+d*d), (b * c - a * d)/(c*c+d*d))


printfn "%A" ((4,0) ./ (2,0))