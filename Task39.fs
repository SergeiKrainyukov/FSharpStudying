// 39.1
let rec rmodd = function
  | [] -> []
  | [x] -> []
  | _::x::tail -> x::(rmodd tail)

let rec rmodd2 = function
  | [] -> []
  | [x] -> [x]
  | x::_::tail -> x::(rmodd2 tail)

// 39.2
let rec del_even = function
 |[]->[]
 |x::tail when x%2=0 -> (del_even tail)
 |x::tail-> x::del_even tail

// 39.3
let rec multiplicity x xs = 
  match xs with
  | [] -> 0
  | head::tail -> (if head=x then 1 else 0) + multiplicity x tail

// 39.4
let rec split = function
  | [] -> ([],[])
  | x -> (rmodd2 x, rmodd x)

// 39.5
exception MyException
let rec zip (xs1,xs2) = 
  match (xs1, xs2) with
  |(xs1, xs2) when List.length xs1 <> List.length xs2 -> raise MyException
  |(xs1, xs2) when xs1 = [] && xs2 = [] -> []
  |(x1::tail1, x2::tail2) -> [(x1,x2)]@zip(tail1, tail2)
  |_->[]