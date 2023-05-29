type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let tr = Node (1, Node (2, Leaf, Leaf), Leaf)

let depth tr =
  let rec maxdepth tr level =
    match tr with
    | Leaf -> level
    | Node (root, left, right) ->
      max (maxdepth left (level+1)) (maxdepth right (level+1))
  in
  maxdepth tr 0

let tree_diameter tr =
  let rec dfs tr mx =
    match tr with
    | Leaf -> mx
    | Node (root, left, right) ->
        let left_diameter = dfs left mx in
        let right_diameter = dfs right mx in
        1 + max left_diameter right_diameter
  in
  dfs tr 0
