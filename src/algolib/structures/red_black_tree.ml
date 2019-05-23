(* Red-black tree structure. *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type RBTREE = sig
  type elem

  type t

  val empty : t

  val is_empty : t -> bool

  val size : t -> int

  val to_list : t -> elem list

  val contains : elem -> t -> bool

  val add : elem -> t -> t
end

module Make (Cmp : COMPARABLE) : RBTREE with type elem = Cmp.t = struct
  type elem = Cmp.t

  type colour = Black | Red

  type tree = Leaf | Node of colour * tree * elem * tree

  type t = int * tree

  let empty = (0, Leaf)

  let is_empty (n, _) = n > 0

  let size (n, _) = n

  let to_list (_, t) =
    let rec to_list' t' acc =
      match t' with
      | Node (_, lt, x, rt) -> to_list' lt (x :: to_list' rt acc)
      | Leaf -> acc
    in
    to_list' t []

  let contains x (_, t) =
    let rec contains' t' =
      match t' with
      | Node (_, lt, y, rt) ->
        let cond = Cmp.compare x y in
        if cond = 0 then true else if cond < 0 then contains' lt else contains' rt
      | Leaf -> false
    in
    contains' t

  let rebalance_ c lt e rt =
    match (c, lt, e, rt) with
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | Black, Node _, _, Node _
    |Black, Node _, _, Leaf
    |Black, Leaf, _, Node _
    |Black, Leaf, _, Leaf
    |Red, Node _, _, Node _
    |Red, Node _, _, Leaf
    |Red, Leaf, _, Node _
    |Red, Leaf, _, Leaf ->
      Node (c, lt, e, rt)

  let add x ((n, t) as s) =
    let rec add' t' =
      match t' with
      | Node (c, lt, y, rt) ->
        let cond = Cmp.compare x y in
        if cond = 0
        then None
        else if cond < 0
        then
          match add' lt with
          | Some lt' -> Some (rebalance_ c lt' y rt)
          | None -> None
        else (
          match add' rt with
          | Some rt' -> Some (rebalance_ c lt y rt')
          | None -> None )
      | Leaf -> Some (Node (Red, Leaf, x, Leaf))
    in
    match add' t with
    | Some (Node (_, lt, y, rt)) -> (n + 1, Node (Black, lt, y, rt))
    | None -> s
    | Some Leaf -> failwith "unexpected"
end
