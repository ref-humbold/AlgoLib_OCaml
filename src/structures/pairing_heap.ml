(* KOPIEC PARUJĄCY *)
module Make(Cmp: COMPARABLE): PAIRING_HEAP =
struct
  module Elem = Cmp
  type t = Null | Node of Elem.t * t list

  exception EmptyHeap

  let create () = Null

  let is_empty h =
    match h with
    | Node _ -> false
    | Null -> true

  let get h =
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

  let merge h1 h2 =
    match (h1, h2) with
    | (Node (e1, hs1), Node (e2, hs2)) ->
      if e1 <= e2
      then Node (e1, h2::hs1)
      else Node (e2, h1::hs2)
    | (Node _ , Null) -> h1
    | (Null, _) -> h2
end
