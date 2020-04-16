(* Structure of pairing heap *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem

  type t

  exception EmptyHeap

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

  exception EmptyHeap

  let empty = Null

  let is_empty h =
    match h with
    | Node _ -> false
    | Null -> true

  let merge h1 h2 =
    match (h1, h2) with
    | Node (e1, hs1), Node (e2, hs2) ->
      if Cmp.compare e1 e2 <= 0 then Node (e1, h2 :: hs1) else Node (e2, h1 :: hs2)
    | Node _, Null -> h1
    | Null, _ -> h2

  let peek h =
    match h with
    | Node (e, _) -> e
    | Null -> raise EmptyHeap

  let push e h = merge h @@ Node (e, [])

  let pop h =
    let rec merge_pairs lst =
      match lst with
      | h1 :: h2 :: hs -> merge (merge h1 h2) @@ merge_pairs hs
      | [h] -> h
      | [] -> Null
    in
    match h with
    | Node (_, hs) -> merge_pairs hs
    | Null -> raise EmptyHeap
end
