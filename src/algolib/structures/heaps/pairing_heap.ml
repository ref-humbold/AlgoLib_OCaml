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

  val merge : t -> t -> t

  val peek : t -> elem

  val push : elem -> t -> t

  val pop : t -> t
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t = struct
  type elem = Cmp.t

  type t = Null | Node of elem * t list

  exception Empty_heap

  let empty = Null

  let is_empty heap =
    match heap with
    | Node _ -> false
    | Null -> true

  let merge heap1 heap2 =
    match (heap1, heap2) with
    | Node (e1, hs1), Node (e2, hs2) ->
      if Cmp.compare e1 e2 <= 0 then Node (e1, heap2 :: hs1) else Node (e2, heap1 :: hs2)
    | Node _, Null -> heap1
    | Null, _ -> heap2

  let peek heap =
    match heap with
    | Node (e, _) -> e
    | Null -> raise Empty_heap

  let push element heap = merge heap @@ Node (element, [])

  let pop heap =
    let rec merge_pairs lst =
      match lst with
      | h1 :: h2 :: hs -> merge (merge h1 h2) @@ merge_pairs hs
      | [h] -> h
      | [] -> Null
    in
    match heap with
    | Node (_, hs) -> merge_pairs hs
    | Null -> raise Empty_heap
end
