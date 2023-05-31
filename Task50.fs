let rec factorial n c =
    if n <= 1 then c 1
    else factorial (n-1) (fun f -> c n * f)

let fac_seq n = seq{
  for i in 0..n do yield factorial i id
}

let seq_seq n = seq{
  for i in 0..n do yield if i % 2 = 0 then i/2 else -(i+1)/2
}