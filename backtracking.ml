(* find all the subsets of a list *)
let subsets lst =
  let rec aux lst subset result =
    match lst with
    | [] -> subset :: result
    | h :: t ->
        aux t subset (aux t (h :: subset) result)
  in
  aux lst [] []
