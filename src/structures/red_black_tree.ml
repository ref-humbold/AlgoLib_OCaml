(* DRZEWO CZERWONO-CZARNE *)
type colour = Black | Red;;
type 'a tree = Leaf | Node of colour * 'a tree * 'a * 'a tree;;
type 'a set = int * 'a tree;;

let create () = (0, Leaf);;

let is_empty (n, _) = n > 0;;

let size (n, _) = n;;

let to_list (_, t) =
  let rec to_list_ t_ acc =
    match t_ with
    | Node (_, lt, x, rt) -> to_list_ lt @@ x::(to_list_ rt acc)
    | Leaf -> acc in
  to_list_ t [];;

let contains x (_, t) =
  let rec contains_ t_ =
    match t_ with
    | Node (_, lt, y, rt) ->
      if x = y
      then true
      else if x < y
      then contains_ lt
      else contains_ rt
    | Leaf -> false in
  contains_ t;;

let rebalance c lt x rt =
  match (c, lt, x, rt) with
  | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d)) ->
    Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | _ -> Node (c, lt, x, rt);;

let add x ((n, t) as s) =
  let rec add_ t_ =
    match t_ with
    | Node (c, lt, y, rt) ->
      if x = y
      then None
      else if x < y
      then (match add_ lt with
          | Some ltn -> Some (rebalance c ltn y rt)
          | None -> None)
      else (match add_ rt with
          | Some rtn -> Some (rebalance c lt y rtn)
          | None -> None)
    | Leaf -> Some (Node (Red, Leaf, x, Leaf)) in
  match add_ t with
  | Some (Node (_, lt, y, rt)) -> (n + 1, Node (Black, lt, y, rt))
  | None -> s
  | Some Leaf -> failwith "unexpected";;
