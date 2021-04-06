(* Structure of real-time Hood-Melville queue *)

type trie_node = Terminus of trie_node array | Node of trie_node array | Empty

type t = {size : int; tree : trie_node}

exception EmptyTrie

let empty = {size = 0; tree = Node (Array.make 256 Empty)}

let is_empty {tree; _} =
  let is_empty' t' =
    match t' with
    | Empty -> true
    | Terminus _ | Node _ -> false
  in
  match tree with
  | Empty -> true
  | Terminus a | Node a -> Array.for_all is_empty' a

let size {size; _} = size

let contains text {tree; _} =
  let rec contains' node i n =
    match node with
    | Empty -> false
    | Terminus a -> if i = n then true else contains' a.(Char.code text.[i]) (i + 1) n
    | Node a -> contains' a.(Char.code text.[i]) (i + 1) n
  in
  contains' tree 0 @@ String.length text

let add text {tree; size} =
  let rec add' node i n =
    if i = n
    then
      match node with
      | Empty -> Terminus (Array.make 256 Empty)
      | Terminus _ -> node
      | Node a -> Terminus a
    else
      match node with
      | Empty ->
        let a = Array.make 256 Empty in
        a.(Char.code text.[i]) <- add' Empty (i + 1) n ;
        Node a
      | Terminus a | Node a ->
        a.(Char.code text.[i]) <- add' a.(Char.code text.[i]) (i + 1) n ;
        node
  in
  {size = size + 1; tree = add' tree 0 @@ String.length text}

let of_seq texts = Seq.fold_left (fun t text -> add text t) empty texts

let of_list texts = List.fold_left (fun t text -> add text t) empty texts
