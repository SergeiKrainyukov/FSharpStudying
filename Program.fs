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




//Task34
// 34.1
let rec upto = function
    | n when n>0 -> 
      let rec upto2 = function
          | (n, array) when n>0 -> upto2(n-1, n::array)
          | (n, array) -> array
      upto2 (n, [])
    |_ -> []

List.iter (fun x -> printfn "%d" x) (upto 5)

// 34.2
let rec dnto = function
  | n when n = 0 -> []
  | n when n > 0 -> n::dnto (n-1)
  |_->[]

List.iter (fun x -> printfn "%d" x) (dnto 5)

// // 34.3
let rec evenn = function
    | n when n>=0 -> 
      let rec upto2 = function
          | (n, array) when n>0 -> upto2(n-1, (n-1)*2::array)
          | (n, array) -> array

      upto2 (n, [])
    |_ -> []

List.iter (fun x -> printfn "%d" x) (evenn 3)




//39.1
// 39.1
let rec rmodd = function
  | [] -> []
  | [x] -> []
  | _::x::tail -> x::(rmodd tail)

List.iter (fun x -> printfn "%d" x) (rmodd [1;2;3;4;5])

let rec rmodd2 = function
  | [] -> []
  | [x] -> [x]
  | x::_::tail -> x::(rmodd2 tail)

List.iter (fun x -> printfn "%d" x) (rmodd2 [1;2;3;4;5])
// 39.2
let rec del_even = function
 |[]->[]
 |x::tail when x%2=0 -> (del_even tail)
 |x::tail-> x::del_even tail

List.iter (fun x -> printfn "%d" x) (del_even [1;2;3;4;5])

// 39.3
let rec multiplicity x xs = 
  match xs with
  | [] -> 0
  | head::tail -> (if head=x then 1 else 0) + multiplicity x tail

printfn "%d" (multiplicity 1 [1;2;3;2;1])

// 39.4
let rec split = function
  | [] -> ([],[])
  | x -> (rmodd2 x, rmodd x)

let list1, list2 = split [1;2;3;4;5;6;7;8;9;10]
List.iter2 (fun x y -> printfn "(%d, %d)" x y) list1 list2

// 39.5
exception MyException
let rec zip (xs1,xs2) = 
  match (xs1, xs2) with
  |(xs1, xs2) when List.length xs1 <> List.length xs2 -> raise MyException
  |(xs1, xs2) when xs1 = [] && xs2 = [] -> []
  |(x1::tail1, x2::tail2) -> [(x1,x2)]@zip(tail1, tail2)
  |_->[]

let xs1 = [1; 2; 3]
let xs2 = ["a"; "b"; "c"]

let zipped = zip (xs1, xs2)
zipped |> List.iter (fun (x, y) -> printfn "(%d, %s)" x y)




//Task40
let rec sum (p, xs) =
    match xs with
    | [] -> 0  
    | x::tail ->   
        let tailSum = sum (p, tail) 
        if p x then x + tailSum  
        else tailSum  


let isEven x = x % 2 = 0 

let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let result = sum (isEven, numbers)

printfn "%d" result


let rec intersect (xs1, xs2) =
    match (xs1, xs2) with
    | [], _ -> []
    | _, [] -> []
    | x::tail1, y::tail2 when x = y -> x :: intersect (tail1, tail2)
    | x::tail1, y::tail2 when x < y -> intersect (tail1, xs2)
    | x::tail1, y::tail2 -> intersect (xs1, tail2)

// Пример использования
let list11 = [1; 2; 3; 4; 5]
let list22 = [3; 4; 5; 6; 7]
let result1 = intersect (list11, list22)

printfn "%A" result1
