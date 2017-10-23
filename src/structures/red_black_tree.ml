(* DRZEWO CZERWONO-CZARNE *)
type colour = Red | Black
type 'a set = Leaf | Node of colour * 'a set * 'a * 'a set

let create () = Leaf

let is_empty s =
  match s with
  | Node _ -> false
  | Leaf -> true

let make_node c lt x rt =
  match (c, lt, x, rt) with
  | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d)) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | _ -> Node (c, lt, x, rt)

let add x t =
  let rec add_ t_ =
    match t_ with
    | Node (c, lt, y, rt) ->
      if x = y
      then t
      else if x < y
      then make_node c (add_ lt) y rt
      else make_node c lt y (add_ rt)
    | Leaf -> Node (Red, Leaf, x, Leaf) in
  match add_ t with
  | Node (_, lt, y, rt) -> Node (Black, lt, y, rt)
  | Leaf -> raise Failure

let rec contains x t =
  match t with
  | Node (_, lt, y, rt) ->
    if x = y
    then true
    else if x < y
    then contains x lt
    else contains x rt
  | Leaf -> false
