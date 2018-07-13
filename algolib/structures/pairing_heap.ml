(* PAIRING HEAP STRUCTURE *)
module type COMPARABLE =
sig
  type c = Less | Equal | Greater
  type t
  val cmp: t -> t -> c
end

module type HEAP =
sig
  type elem
  type t
  exception EmptyHeap
  val empty: t
  val is_empty: t -> bool
  val merge: t -> t -> t
  val peek: t -> elem
  val push: elem -> t -> t
  val pop: t -> t
end

module Make(Cmp: COMPARABLE) =
struct
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
    | (Node (e1, hs1), Node (e2, hs2)) ->
      (match Cmp.cmp e1 e2 with
       | Less | Equal -> Node (e1, h2::hs1)
       | Greater ->  Node (e2, h1::hs2))
    | (Node _ , Null) -> h1
    | (Null, _) -> h2

  let peek h =
    match h with
    | Node (e, _) -> e
    | Null -> raise EmptyHeap

  let push e h = merge h @@ Node (e, [])

  let pop h =
    let rec merge_pairs lst =
      match lst with
      | h1::h2::hs -> merge (merge h1 h2) @@ merge_pairs hs
      | [h] -> h
      | [] -> Null in
    match h with
    | Node (_, hs) -> merge_pairs hs
    | Null -> raise EmptyHeap
end
