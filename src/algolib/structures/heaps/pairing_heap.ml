(* Structure of pairing heap *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem

  type t

  exception Empty_heap

  val empty : t

  val is_empty : t -> bool

  val length : t -> int

  val merge : t -> t -> t

  val peek : t -> elem

  val peek_opt : t -> elem option

  val push : elem -> t -> t

  val pop : t -> t

  val of_seq : elem Seq.t -> t

  val of_list : elem list -> t
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t = struct
  type elem = Cmp.t

  type node = Null | Node of elem * node list

  type t = {size : int; node : node}

  exception Empty_heap

  let empty = {size = 0; node = Null}

  let is_empty heap =
    match heap.node with
    | Node _ -> false
    | Null -> true

  let length heap = heap.size

  let merge_ node1 node2 =
    match (node1, node2) with
    | Node (e1, hs1), Node (e2, hs2) ->
      if Cmp.compare e1 e2 <= 0 then Node (e1, node2 :: hs1) else Node (e2, node1 :: hs2)
    | Node _, Null -> node1
    | Null, _ -> node2

  let merge {size = size1; node = node1} {size = size2; node = node2} =
    {size = size1 + size2; node = merge_ node1 node2}

  let peek_opt heap =
    match heap.node with
    | Node (e, _) -> Some e
    | Null -> None

  let peek heap =
    match peek_opt heap with
    | Some e -> e
    | None -> raise Empty_heap

  let push element {size; node} = {size = size + 1; node = merge_ node @@ Node (element, [])}

  let pop {size; node} =
    let rec merge_pairs lst =
      match lst with
      | h1 :: h2 :: hs -> merge_ (merge_ h1 h2) @@ merge_pairs hs
      | [h] -> h
      | [] -> Null
    in
    match node with
    | Node (_, hs) -> {size = size - 1; node = merge_pairs hs}
    | Null -> raise Empty_heap

  let of_seq xs = Seq.fold_left (fun acc e -> push e acc) empty xs

  let of_list xs = List.fold_left (fun acc e -> push e acc) empty xs
end
