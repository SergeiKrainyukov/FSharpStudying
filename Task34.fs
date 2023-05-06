// 34.1
let rec upto = function
    | n when n>0 -> 
      let rec upto2 = function
          | (n, array) when n>0 -> upto2(n-1, n::array)
          | (n, array) -> array
      upto2 (n, [])
    |_ -> []

// 34.2
let rec dnto = function
  | n when n = 0 -> []
  | n when n > 0 -> n::dnto (n-1)
  |_->[]

// 34.3
let rec evenn = function
    | n when n>=0 -> 
      let rec upto2 = function
          | (n, array) when n>0 -> upto2(n-1, (n-1)*2::array)
          | (n, array) -> array
      upto2 (n, [])
    |_ -> []