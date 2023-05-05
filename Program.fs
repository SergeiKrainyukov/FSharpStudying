module SomeNamespace.SomeModule'

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
let (./) (a, b) (c, d) = ( (a*c+b*d)/(c*c+d*d), (b * c - a * d)/(c*c+d*d))


printfn "%A" ((4,0) ./ (2,0))





// type TimeOfDay = { hours: int; minutes: int; f: string }

// let more = function
//   | x, y when (x.f = "PM") && (y.f = "AM") -> true
//   | x, y when (x.f = "AM") && (y.f = "PM") -> false
//   | x, y when (x.f = y.f) && (x.hours>y.hours) -> true
//   | x, y when (x.f = y.f) && (x.hours<y.hours) -> false
//   | x, y when (x.f = y.f) && (x.hours=y.hours) && (x.minutes>y.minutes)-> true
//   | x, y when (x.f = y.f) && (x.hours=y.hours) && (x.minutes<y.minutes)-> false
//   |_ -> false

// let (.>.) x y = more (x, y)

// let v1 = { hours = 2; minutes = 19; f = "PM" }
// let v2 = { hours = 2; minutes = 19; f = "PM" }
// // true 

// printfn "%b" (v1.>.v2)





type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let transform (x: TimeOfDay) = 
    if x.f = PM then (x.hours + 12) * 60 + x.minutes
    else x.hours * 60 + x.minutes

let (.>.) x y = (transform x) > (transform y)

let v1 = { hours = 3; minutes = 0; f = PM }
let v2 = { hours = 2; minutes = 59; f = PM }

printfn "%b" (v1.>.v2)