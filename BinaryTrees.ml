type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let tr = Node (1, Node (2, Leaf, Leaf), Leaf)

let node_val node =
  match node with
  | Leaf -> None
  | Node (k, _, _) -> Some k

(*Find the max depth of a binary tree*)
let depth tr =
  let rec maxdepth tr level =
    match tr with
    | Leaf -> level
    | Node (root, left, right) ->
      max (maxdepth left (level+1)) (maxdepth right (level+1))
  in
  maxdepth tr 0

(*Find the diameter of a binary tree*)
let rec dfs tr mx =
  match tr with
  | Leaf -> mx
  | Node (root, left, right) ->
      let left_diameter = dfs left mx in
      let right_diameter = dfs right mx in
      1 + max left_diameter right_diameter

let tree_diameter tr =
  match tr with
  | Leaf -> (-1)
  | Node (root, Leaf, Leaf) -> 0
  | Node (root, left, Leaf) -> dfs tr (-1)
  | Node (root, Leaf, right) -> dfs tr (-1)
  | Node (root, left, right) -> dfs tr 0

(*Invert a binary tree*)
let rec invert tr =
  match tr with
  | Leaf -> Leaf
  | Node (root, Leaf, Leaf) -> Node (root, Leaf, Leaf)
  | Node (root, left, right) -> Node (root, invert right, invert left)

(*Balanced Binary tree*)
let rec is_balanced tr =
  match tr with
  | Leaf -> true
  | Node (root, left, right) ->
      if abs(depth left - depth right) > 1 then false
      else true

(*Is Same Tree*)
let rec is_same_tree tr1 tr2 =
  match (tr1, tr2) with
  | Leaf, Leaf -> true
  | Node (root1, left1, right1), Node (root2, left2, right2) ->
      if root1 = root2 
      then (is_same_tree left1 left2) && (is_same_tree right1 right2)
      else false
  | _ -> false

(*Subtree*)
let rec is_subtree tr subtr =
  match (tr, subtr) with
  | _, Leaf -> true
  | Leaf, _ -> false
  | Node (root, left, right), Node (subroot, sleft, sright) ->
      is_same_tree tr subtr || 
      is_subtree left (Node (subroot, sleft, sright)) || 
      is_subtree right (Node (subroot, sleft, sright))

(*Lowest Common ancestor of a Binary Search Tree*)
let rec lca_bst tr node1 node2 =
  match tr with
  | Leaf -> Leaf
  | Node (root, left, right) ->
      if (node_val node1) > (node_val root) &&
      (node_val node2) > (node_val root)
      then lca_bst right node1 node2
      else if (node_val node1) < (node_val root) &&
      (node_val node2) < (node_val root)
      then lca_bst left node1 node2
      else root

(*Tree level order traversal*)
let level_traversal tr =
  let rec aux tr accum =
    match tr with
    | Leaf -> List.rev accum
    | Node (root, Leaf, Leaf) -> List.rev [root]::accum
    | Node (root, left, right) ->
        aux left (aux right ([root]::accum)) 
  in
  aux tr []

(*Good nodes of binary tree*)
let good_nodes tr =
  let rec aux tr max_val =
    match tr with
    | Leaf -> 0
    | Node (value, left, right) ->
        if value >= max_val
        then 1 + (aux left value) + (aux right value)
        else (aux left max_val) + (aux right max_val)
  in
  match tr with
  | Leaf -> 0
  | Node (value, l, r) -> aux tr value
