// 49.5.1
let even_seq = Seq.initInfinite (fun i -> i*2+2)

let rec factorial n c =
    if n <= 1 then c 1
    else factorial (n-1) (fun f -> c n * f)

// 49.5.2
let fac_seq = Seq.initInfinite (fun i -> factorial i id)

// 49.5.3
let seq_seq = Seq.initInfinite (fun i -> if i % 2 = 0 then -i/2 else (i+1)/2)