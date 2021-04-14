(* Structure of trie *)

type trie_node = Terminus of trie_node array | Node of trie_node array | Empty

type t = {tree : trie_node; size : int}

let empty = {tree = Node (Array.make 256 Empty); size = 0}

let set_array_ node a =
  match node with
  | Node _ -> Node a
  | Terminus _ -> Terminus a
  | Empty -> node

let is_empty_node_ node =
  let is_empty' node' =
    match node' with
    | Empty -> true
    | Terminus _ | Node _ -> false
  in
  match node with
  | Empty -> true
  | Terminus a | Node a -> Array.for_all is_empty' a

let is_empty {tree; _} = is_empty_node_ tree

let size {size; _} = size

let contains text {tree; _} =
  let rec contains' node' i' =
    if i' = String.length text
    then
      match node' with
      | Empty | Node _ -> false
      | Terminus _ -> true
    else
      match node' with
      | Empty -> false
      | Terminus a | Node a -> contains' a.(Char.code text.[i']) (i' + 1)
  in
  contains' tree 0

let add text t =
  let rec add' node' i' =
    if i' = String.length text
    then
      match node' with
      | Empty -> Terminus (Array.make 256 Empty)
      | Terminus _ -> node'
      | Node a -> Terminus a
    else
      match node' with
      | Empty ->
        let a = Array.make 256 Empty in
        a.(Char.code text.[i']) <- add' Empty (i' + 1) ;
        Node a
      | Terminus a | Node a ->
        let old_node = a.(Char.code text.[i']) in
        let new_node = add' old_node (i' + 1) in
        if new_node == old_node
        then node'
        else
          let new_array = Array.copy a in
          new_array.(Char.code text.[i']) <- new_node ;
          set_array_ node' new_array
  in
  let new_tree = add' t.tree 0 in
  if t.tree == new_tree then t else {tree = new_tree; size = t.size + 1}

let remove text t =
  let rec remove_node' node' i' =
    if i' = String.length text
    then
      match node' with
      | Terminus a -> if is_empty_node_ node' then Empty else Node a
      | Node _ | Empty -> node'
    else if i' < String.length text
    then (
      match node' with
      | Empty -> node'
      | Node a | Terminus a ->
        let old_node = a.(Char.code text.[i']) in
        let new_node = remove_node' a.(Char.code text.[i']) (i' + 1) in
        if old_node == new_node
        then if is_empty_node_ node' then Empty else node'
        else
          let new_array = Array.copy a in
          new_array.(Char.code text.[i']) <- new_node ;
          if is_empty_node_ node' then Empty else set_array_ node' new_array )
    else if is_empty_node_ node'
    then Empty
    else node'
  in
  let new_tree = remove_node' t.tree 0 in
  if t.tree == new_tree then t else {tree = new_tree; size = t.size - 1}

let of_seq texts = Seq.fold_left (fun t text -> add text t) empty texts

let of_list texts = List.fold_left (fun t text -> add text t) empty texts
