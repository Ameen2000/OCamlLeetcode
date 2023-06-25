exception Empty
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

(* permutations *)
let rec delete elem lst =
  match lst with
  | [] -> raise Empty
  | x :: xs ->
      if x = elem
      then xs
      else x :: (delete elem xs)

let rec permute lst =
  let n = List.length lst in
  if n = 1
  then [lst]
  else
  let rec aux k =
    let elem = List.nth lst k in
    let subperm = permute (delete elem lst) in
    let t = List.map (fun a -> elem :: a) subperm in
    if k < n - 1
    then List.rev_append t (aux (k + 1))
    else t
  in
  aux 0
