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