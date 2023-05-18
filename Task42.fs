let rec allSubsets n k =
    if k = 0 then
       Set.singleton Set.empty
    elif n = 0 then
       Set.empty
    else
        let subsetsWithN = allSubsets (n - 1) (k - 1) |> Set.map (fun subset -> Set.add n subset)
        let subsetsWithoutN = allSubsets (n - 1) k
        Set.union subsetsWithN subsetsWithoutN
