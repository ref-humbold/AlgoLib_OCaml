(* Leftist heap structure. *)
module type COMPARABLE =
sig
  type t
  val compare: t -> t -> int
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

  type t = Null | Node of int * elem * t * t

  exception EmptyHeap

  let empty = Null

  let is_empty h =
    match h with
    | Node _ -> false
    | Null -> true

  let rec merge h1 h2 =
    let rank v =
      match v with
      | Node (r, _, _, _) -> r
      | Null -> 0 in
    let make_node x a b =
      if rank a < rank b
      then Node ((rank a) + 1, x, b, a)
      else Node ((rank b) + 1, x, a, b) in
    match h1, h2 with
    | Node (_, x, lt1, rt1), Node (_, y, lt2, rt2) ->
      if Cmp.compare x y < 0
      then make_node x lt1 @@ merge rt1 h2
      else make_node y lt2 @@ merge rt2 h1
    | Node (_, _, _, _), Null -> h1
    | Null, _ -> h2

  let peek v =
    match v with
    | Node (_, x, _, _) -> x
    | Null -> raise EmptyHeap

  let push x h = merge h @@ Node (1, x, Null, Null)

  let pop v =
    match v with
    | Node (_, _, lt, rt) -> merge lt rt
    | Null -> raise EmptyHeap
end
