let rec nub lst =
  match lst with
  | [] as l -> l
  | [_] as l -> l
  | h :: t ->
      if List.mem h t
      then nub t
      else h :: (nub t)
(* find all the subsets of a list *)
let subsets lst =
  let rec aux lst subset result =
    match lst with
    | [] -> subset :: result
    | h :: t ->
        aux t subset (aux t (h :: subset) result)
  in
  aux lst [] []

(* combination sum *)
let combination_sum lst target =
  let rec aux lst cur total result =
    match lst with
    | [] -> result
    | x :: xs ->
        if (total + x) = target
        then aux (x :: xs) (x :: cur) (total + x) ((x :: cur) :: result)
        else if (total + x) < target
        then aux xs cur total (aux (x :: xs) (x :: cur) (total + x) result)
        else aux xs cur total result
  in
  aux lst [] 0 []
