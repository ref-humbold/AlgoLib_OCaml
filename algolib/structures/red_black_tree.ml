(** RED-BLACK TREE STRUCTURE *)
module type COMPARABLE =
sig
  type t
  val cmp: t -> t -> int
end

module type RED_BLACK_TREE =
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

module Make(Cmp: COMPARABLE): (RED_BLACK_TREE with type elem = Cmp.t) =
struct
  type elem = Cmp.t
  type colour = Black | Red
  type rbtree = Leaf | Node of colour * rbtree * elem * rbtree
  type t = int * rbtree

  let empty = (0, Leaf)

  let is_empty (n, _) = n > 0

  let size (n, _) = n

  let to_list (_, t) =
    let rec to_list_ tx acc =
      match tx with
      | Node (_, lt, x, rt) -> to_list_ lt @@ x::(to_list_ rt acc)
      | Leaf -> acc in
    to_list_ t []

  let contains x (_, t) =
    let rec contains_ tx =
      match tx with
      | Node (_, lt, y, rt) ->
        let cmp_res = Cmp.cmp x y in
        if cmp_res = 0
        then true
        else if cmp_res < 0
        then contains_ lt
        else contains_ rt
      | Leaf -> false in
    contains_ t

  let rebalance c lt e rt =
    match (c, lt, e, rt) with
    | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d)) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | (Black, Node _, _, Node _)| (Black, Node _, _, Leaf)
    | (Black, Leaf, _, Node _) | (Black, Leaf, _, Leaf)
    | (Red, Node _, _, Node _) | (Red, Node _, _, Leaf)
    | (Red, Leaf, _, Node _) | (Red, Leaf, _, Leaf) -> Node (c, lt, e, rt)

  let add x ((n, t) as s) =
    let rec add_ tx =
      match tx with
      | Node (c, lt, y, rt) ->
        let cmp_res = Cmp.cmp x y in
        if cmp_res = 0
        then None
        else if cmp_res < 0
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
    | Some Leaf -> failwith "UNEXPECTED"
end
