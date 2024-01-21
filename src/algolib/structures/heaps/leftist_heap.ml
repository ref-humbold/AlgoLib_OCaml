(* Structure of leftist heap. *)
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

  type node = Null | Node of {left : node; rank : int; element : elem; right : node}

  type t = {size : int; node : node}

  exception Empty_heap

  let empty = {size = 0; node = Null}

  let is_empty heap =
    match heap.node with
    | Node _ -> false
    | Null -> true

  let length heap = heap.size

  let rec merge_ heap1 heap2 =
    let rank' node =
      match node with
      | Node {rank; _} -> rank
      | Null -> 0
    in
    let make_node element node1 node2 =
      if rank' node1 < rank' node2
      then Node {left = node2; rank = rank' node1 + 1; element; right = node1}
      else Node {left = node1; rank = rank' node2 + 1; element; right = node2}
    in
    match (heap1, heap2) with
    | ( Node {left = lt1; element = e1; right = rt1; _},
        Node {left = lt2; element = e2; right = rt2; _} ) ->
      if Cmp.compare e1 e2 < 0
      then make_node e1 lt1 @@ merge_ rt1 heap2
      else make_node e2 lt2 @@ merge_ rt2 heap1
    | Node _, Null -> heap1
    | Null, _ -> heap2

  let merge {size = size1; node = node1} {size = size2; node = node2} =
    {size = size1 + size2; node = merge_ node1 node2}

  let peek_opt heap =
    match heap.node with
    | Node {element; _} -> Some element
    | Null -> None

  let peek heap =
    match peek_opt heap with
    | Some e -> e
    | None -> raise Empty_heap

  let push element {size; node} =
    {size = size + 1; node = merge_ node @@ Node {left = Null; rank = 1; element; right = Null}}

  let pop {size; node} =
    match node with
    | Node {left; right; _} -> {size = size - 1; node = merge_ left right}
    | Null -> raise Empty_heap

  let of_seq xs = Seq.fold_left (fun acc e -> push e acc) empty xs

  let of_list xs = List.fold_left (fun acc e -> push e acc) empty xs
end
