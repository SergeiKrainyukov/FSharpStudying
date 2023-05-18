let rec allSubsets n k =
    if k = 0 then
        [ [] ]
    elif n = 0 then
        []
    else
        let subsetsWithN = allSubsets (n - 1) (k - 1) |> List.map (fun subset -> n :: subset)
        let subsetsWithoutN = allSubsets (n - 1) k
        subsetsWithN @ subsetsWithoutN
