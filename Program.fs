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
// let rec sum (p, xs) =
//     match xs with
//     | [] -> 0  
//     | x::tail ->   
//         let tailSum = sum (p, tail) 
//         if p x then x + tailSum  
//         else tailSum  


// let isEven x = x % 2 = 0 

// let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
// let result = sum (isEven, numbers)

// printfn "%d" result


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




let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | [], xs2 -> xs2 
    | xs1, [] -> xs1  
    | x::tail1, y::tail2 when x = y -> x :: y :: plus (tail1, tail2)  
    | x::tail1, y::tail2 when x < y -> x :: plus (tail1, xs2)  
    | x::tail1, y::tail2 -> y :: plus (xs1, tail2)


let list111 = [0; 1; 1; 2]
let list222 = [1; 2; 2; 3]
let result11 = plus (list111, list222)

printfn "%A" result11



let minus x y = x - y
let a = List.fold minus 0 [1; 2; 3]
printfn "%d" a


// 41.4.1
let list_filter f xs =
    List.foldBack
        (fun x acc -> if f x then x::acc else acc)
        xs
        []


// 41.4.2
let sum (p, xs) = 
  let rec checkPredicate  = function
    | [] -> []
    | (x::tail) when p x = true -> x::checkPredicate (tail)
    | (x::tail) when p x = false -> checkPredicate (tail)
    |_->[]
  List.fold (+) 0 (checkPredicate xs)

printfn "%d" (sum ((fun x -> x > 0), [-1; 2; 1]))

// 41.4.3
let revrev = 
  let rev lst = List.fold (fun head tail -> tail::head) [] lst
  function
    | []->[]
    | lists -> List.fold(fun head tail -> rev tail::rev head) [] (lists)

printfn "%A" (revrev [[1;2];[3;4;5]])



// let rec allSubsets n k =
//   match (n,k) with
//   |(n,k) when n = 0 || k = 0 -> set []
//   |(n,k) when n=1&&k=1->set [1]


// let rec allSubsets n k =
//     if k = 0 then
//        Set.singleton Set.empty
//     elif n = 0 then
//        Set.empty
//     else
//         let subsetsWithN = allSubsets (n - 1) (k - 1) |> Set.map (fun subset -> Set.add n subset)
//         let subsetsWithoutN = allSubsets (n - 1) k
//         Set.union subsetsWithN subsetsWithoutN




// let rec subsets = function
//     | [] -> [[]]
//     | head::tail -> 
//         let tailSubsets = subsets tail
//         let prependHead subset = head::subset
//         let prependEmpty subset = subset
//         let subsetsWithHead = List.map prependHead tailSubsets
//         let subsetsWithoutHead = List.map prependEmpty tailSubsets
//         subsetsWithHead @ subsetsWithoutHead


let rec test xs = 
  match xs with
    | [] -> [[]]
    | head::tail -> List.fold(fun a b -> (head::b)::(b::a) ) [] (test tail)

printfn "%A" (test [1,2,3])

let rec subsets xs= 
         match xs with 
           | [] -> [[]] 
           | head::tail -> List.fold (fun head1 tail1 -> 
                (head::tail1)::tail1::head1) [] (subsets tail) 
// printfn "%A" (subsets [1,2,3])


let allSubsets n k = 
     Set.filter (fun xs -> true) (Set.ofList (List.foldBack (fun head tail ->
        (Set.ofList head) :: tail) (subsets [1..n]) [] )) 


printfn "%A" (allSubsets 2 2)

let a2 = [(128,"oksana"); (32,"oleg")]

// 43.3
let try_find key m = 
  let getSecond = function
    | (_, sec) -> sec

  let isExist = function
    | (key, keyValue) when key = (fst keyValue) -> true
    |_-> false

  let rec find = function
    | [] -> None
    | head::_ when isExist (key, head) = true -> Some (getSecond (head))
    | _::tail -> find tail
  find (Map.toList m)

let map = Map.ofList [(1, "One"); (2, "Two"); (3, "Three")]
let result = try_find 1 map

match result with
| Some value -> printfn "%s" value
| None -> printfn "Key not found"


