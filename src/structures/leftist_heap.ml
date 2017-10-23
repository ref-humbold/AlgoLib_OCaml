(* KOPIEC LEWICOWY *)
type 'a heap = Null | Vertex of int * 'a * 'a heap * 'a heap

exception EmptyHeap

let create () = Null

let is_empty h =
  match h with
  | Vertex _ -> false
  | Null -> true

let rank v =
  match v with
  | Vertex (r, _, _, _) -> r
  | Null -> 0

let make_vertex x a b =
  if rank a < rank b
  then Vertex ((rank a) + 1, x, b, a)
  else Vertex ((rank b) + 1, x, a, b)

let get v =
  match v with
  | Vertex (_, x, _, _) -> x
  | Null -> raise EmptyHeap

let rec merge h1 h2 =
  match (h1, h2) with
  | (Vertex (_, x, lt1, rt1), Vertex (_, y, lt2, rt2)) ->
    if x < y
    then make_vertex x lt1 (merge rt1 h2)
    else make_vertex y lt2 (merge rt2 h1)
  | (Vertex (_, _, _, _), Leaf) -> h
  | (Leaf, h) -> h

let push x h = merge h @@ Vertex (1, x, Null, Null)

let pop v =
  match v with
  | Vertex (_, _, lt, rt) -> merge lt rt
  | Null -> raise EmptyHeap
