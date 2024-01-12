(* Structure of leftist heap *)
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

  val merge : t -> t -> t

  val peek : t -> elem

  val push : elem -> t -> t

  val pop : t -> t
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t = struct
  type elem = Cmp.t

  type t = Null | Node of {left : t; rank : int; element : elem; right : t}

  exception Empty_heap

  let empty = Null

  let is_empty heap =
    match heap with
    | Node _ -> false
    | Null -> true

  let rec merge heap1 heap2 =
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
      then make_node e1 lt1 @@ merge rt1 heap2
      else make_node e2 lt2 @@ merge rt2 heap1
    | Node _, Null -> heap1
    | Null, _ -> heap2

  let peek heap =
    match heap with
    | Node {element; _} -> element
    | Null -> raise Empty_heap

  let push element heap = merge heap @@ Node {left = Null; rank = 1; element; right = Null}

  let pop heap =
    match heap with
    | Node {left; right; _} -> merge left right
    | Null -> raise Empty_heap
end
