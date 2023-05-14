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

// 41.4.3
let revrev = 
  let rev lst = List.fold (fun head tail -> tail::head) [] lst
  function
    | []->[]
    | lists -> List.fold(fun head tail -> rev tail::rev head) [] (lists)