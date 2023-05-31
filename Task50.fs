let rec factorial n c =
    if n <= 1 then c 1
    else factorial (n-1) (fun f -> c n * f)

let rec facSeq n = seq{
  yield factorial n id
  yield! facSeq (n + 1)
}

let fac_seq = facSeq 0

let rec seqSeq i = seq{
  yield if i % 2 = 0 then i/2 else -(i+1)/2
  yield! seqSeq (i+1)
}

let seq_seq = seqSeq 0