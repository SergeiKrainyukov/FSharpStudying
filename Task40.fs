let rec sum (p, xs) =
    match xs with
    | [] -> 0  
    | x::tail ->   
        let tailSum = sum (p, tail) 
        if p x then x + tailSum  
        else tailSum 

let rec count (xs, n) =
    match xs with
    | [] -> 0  
    | x::tail when x = n -> 1 + count (tail, n)
    | x::tail when x > n -> 0  
    | _::tail -> count (tail, n)

let rec insert (xs, n) =
    match xs with
    | [] -> [n]
    | x::tail when x >= n -> n :: xs  
    | x::tail -> x :: insert (tail, n)  

let rec intersect (xs1, xs2) =
    match (xs1, xs2) with
    | [], _ -> []
    | _, [] -> []
    | x::tail1, y::tail2 when x = y -> x :: intersect (tail1, tail2)
    | x::tail1, y::tail2 when x < y -> intersect (tail1, xs2)
    | x::tail1, y::tail2 -> intersect (xs1, tail2)

let rec plus (xs1, xs2) =
    match (xs1, xs2) with
    | [], xs2 -> xs2 
    | xs1, [] -> xs1  
    | x::tail1, y::tail2 when x = y -> x :: y :: plus (tail1, tail2)  
    | x::tail1, y::tail2 when x < y -> x :: plus (tail1, xs2)  
    | x::tail1, y::tail2 -> y :: plus (xs1, tail2)

let rec minus (xs1, xs2) =
    match (xs1, xs2) with
    | [], _ -> [] 
    | xs1, [] -> xs1 
    | x::tail1, y::tail2 when x = y -> minus (tail1, xs2) 
    | x::tail1, y::tail2 when x < y -> x :: minus (tail1, xs2)
    | x::tail1, y::tail2 -> minus (xs1, tail2)


let rec smallest xs =
    match xs with
    | [] -> None  
    | [x] -> Some x  
    | x::tail ->
        let rest = smallest tail 
        match rest with
        | None -> Some x  
        | Some y -> Some (min x y)

let rec delete (n, xs) =
    match xs with
    | [] -> []
    | y::tail ->
        if y = n then tail 
        else y :: delete (n, tail)


let rec sort xs =
    let rec insert (x, sorted) =
        match sorted with
        | [] -> [x]
        | y::ys ->
            if x <= y then x :: y :: ys 
            else y :: insert (x, ys)

    match xs with
    | [] -> []  
    | x::xs' -> insert (x, sort xs') 


let revrev lists =
    let reverseList = List.rev
    let reverseInnerLists = List.map reverseList
    reverseInnerLists (reverseList lists)




 

 
