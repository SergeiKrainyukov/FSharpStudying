let rec fibo1 n n1 n2 =
  if n = 0 then n2
  else fibo1 (n-1) (n1+n2) (n1)

let rec fibo2 n c =
  if n = 0 then c 0
  elif n = 1 then c 1
  else fibo2 (n-1) (fun last -> fibo2 (n-2) (fun last2 -> c (last+last2)))

let rec bigList n k =
  if n=0 then k []
  else bigList (n-1) (fun res -> k(1::res))