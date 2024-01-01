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

  type t = Null | Node of {rank : int; e : elem; lt : t; rt : t}

  exception Empty_heap

  let empty = Null

  let is_empty h =
    match h with
    | Node _ -> false
    | Null -> true

  let rec merge h1 h2 =
    let rank' v =
      match v with
      | Node {rank; _} -> rank
      | Null -> 0
    in
    let make_node e a b =
      if rank' a < rank' b
      then Node {rank = rank' a + 1; e; lt = b; rt = a}
      else Node {rank = rank' b + 1; e; lt = a; rt = b}
    in
    match (h1, h2) with
    | Node {e = x; lt = lt1; rt = rt1; _}, Node {e = y; lt = lt2; rt = rt2; _} ->
      if Cmp.compare x y < 0
      then make_node x lt1 @@ merge rt1 h2
      else make_node y lt2 @@ merge rt2 h1
    | Node _, Null -> h1
    | Null, _ -> h2

  let peek v =
    match v with
    | Node {e; _} -> e
    | Null -> raise Empty_heap

  let push e h = merge h @@ Node {rank = 1; e; lt = Null; rt = Null}

  let pop v =
    match v with
    | Node {lt; rt; _} -> merge lt rt
    | Null -> raise Empty_heap
end
