(* RED-BLACK TREE STRUCTURE *)
module type COMPARABLE =
sig
  type t
  type c = Less | Equal | Greater
  val cmp: t -> t -> c
end

module type TREE =
sig
  type elem
  type t
  val empty: t
  val is_empty: t -> bool
  val size: t -> int
  val to_list: t -> elem list
  val contains: elem -> t -> bool
  val add: elem -> t -> t
end

module Make(Cmp: COMPARABLE) =
struct
  type elem = Cmp.t
  type colour = Black | Red
  type tree = Leaf | Node of colour * tree * elem * tree
  type t = int * tree

  let empty = (0, Leaf)

  let is_empty (n, _) = n > 0

  let size (n, _) = n

  let to_list (_, t) =
    let rec to_list' tx acc =
      match tx with
      | Node (_, lt, x, rt) -> to_list' lt @@ x::(to_list' rt acc)
      | Leaf -> acc in
    to_list' t []

  let contains x (_, t) =
    let rec contains' tx =
      match tx with
      | Node (_, lt, y, rt) ->
        ( match Cmp.cmp x y with
          | Cmp.Equal -> true
          | Cmp.Less -> contains' lt
          | Cmp.Greater -> contains' rt
        )
      | Leaf -> false in
    contains' t

  let rebalance_ c lt e rt =
    match c, lt, e, rt with
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, Node _, _, Node _ | (Black, Node _, _, Leaf)
    | Black, Leaf, _, Node _ | (Black, Leaf, _, Leaf)
    | Red, Node _, _, Node _ | (Red, Node _, _, Leaf)
    | Red, Leaf, _, Node _ | (Red, Leaf, _, Leaf) -> Node (c, lt, e, rt)

  let add x ((n, t) as s) =
    let rec add_ tx =
      match tx with
      | Node (c, lt, y, rt) ->
        ( match Cmp.cmp x y with
          | Cmp.Equal -> None
          | Cmp.Less ->
            ( match add_ lt with
              | Some ltn -> Some (rebalance_ c ltn y rt)
              | None -> None
            )
          | Cmp.Greater ->
            ( match add_ rt with
              | Some rtn -> Some (rebalance_ c lt y rtn)
              | None -> None
            )
        )
      | Leaf -> Some (Node (Red, Leaf, x, Leaf)) in
    match add_ t with
    | Some (Node (_, lt, y, rt)) -> (n + 1, Node (Black, lt, y, rt))
    | None -> s
    | Some Leaf -> failwith "UNEXPECTED"
end
