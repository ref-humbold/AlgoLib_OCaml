(* Structure of red-black tree. *)
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

  val contains : elem -> t -> bool

  val add : elem -> t -> t

  val add_seq : elem Seq.t -> t -> t

  val add_list : elem list -> t -> t

  val of_seq : elem Seq.t -> t

  val of_list : elem list -> t

  val to_seq : t -> elem Seq.t

  val to_rev_seq : t -> elem Seq.t

  val to_list : t -> elem list

  val to_rev_list : t -> elem list
end

module Make (Cmp : COMPARABLE) : RBTREE with type elem = Cmp.t = struct
  type elem = Cmp.t

  type colour = Black | Red

  type node = Leaf | Node of {col : colour; left : node; e : elem; right : node}

  type t = {size : int; node : node}

  let empty = {size = 0; node = Leaf}

  let is_empty tree = tree.node = Leaf

  let size tree = tree.size

  let contains x tree =
    let rec contains' n =
      match n with
      | Node {left; e; right; _} ->
        let cond = Cmp.compare x e in
        if cond = 0 then true else if cond < 0 then contains' left else contains' right
      | Leaf -> false
    in
    contains' tree.node

  let rebalance_ col left e right =
    match (col, left, e, right) with
    | ( Black,
        Node {col = Red; left = Node {col = Red; left = ltx; e = x; right = rtx}; e = y; right = rty},
        z,
        d ) ->
      Node
        { col = Red;
          left = Node {col = Black; left = ltx; e = x; right = rtx};
          e = y;
          right = Node {col = Black; left = rty; e = z; right = d} }
    | ( Black,
        Node {col = Red; left = ltx; e = x; right = Node {col = Red; left = lty; e = y; right = rty}},
        z,
        d ) ->
      Node
        { col = Red;
          left = Node {col = Black; left = ltx; e = x; right = lty};
          e = y;
          right = Node {col = Black; left = rty; e = z; right = d} }
    | ( Black,
        ltx,
        x,
        Node {col = Red; left = lty; e = y; right = Node {col = Red; left = ltz; e = z; right = rtz}}
      ) ->
      Node
        { col = Red;
          left = Node {col = Black; left = ltx; e = x; right = lty};
          e = y;
          right = Node {col = Black; left = ltz; e = z; right = rtz} }
    | ( Black,
        ltx,
        x,
        Node {col = Red; left = Node {col = Red; left = lty; e = y; right = rty}; e = z; right = rtz}
      ) ->
      Node
        { col = Red;
          left = Node {col = Black; left = ltx; e = x; right = lty};
          e = y;
          right = Node {col = Black; left = rty; e = z; right = rtz} }
    | Black, Node _, _, Node _
    | Black, Node _, _, Leaf
    | Black, Leaf, _, Node _
    | Black, Leaf, _, Leaf
    | Red, Node _, _, Node _
    | Red, Node _, _, Leaf
    | Red, Leaf, _, Node _
    | Red, Leaf, _, Leaf -> Node {col; left; e; right}

  let add x tree =
    let rec add' t' =
      match t' with
      | Node {col; left; e; right} ->
        let cond = Cmp.compare x e in
        if cond = 0
        then None
        else if cond < 0
        then
          match add' left with
          | Some left' -> Some (rebalance_ col left' e right)
          | None -> None
        else (
          match add' right with
          | Some right' -> Some (rebalance_ col left e right')
          | None -> None )
      | Leaf -> Some (Node {col = Red; left = Leaf; e = x; right = Leaf})
    in
    match add' tree.node with
    | Some (Node nd) -> {size = tree.size + 1; node = Node {nd with col = Black}}
    | None -> tree
    | Some Leaf -> failwith "unexpected"

  let add_list xs tree = List.fold_left (fun acc x -> add x acc) tree xs

  let add_seq xs tree = Seq.fold_left (fun acc x -> add x acc) tree xs

  let of_seq xs = add_seq xs empty

  let of_list xs = add_list xs empty

  let to_seq tree =
    let rec to_seq' n acc =
      match n with
      | Node {left; e; right; _} -> to_seq' left @@ Seq.cons e @@ to_seq' right acc
      | Leaf -> acc
    in
    to_seq' tree.node Seq.empty

  let to_rev_seq tree =
    let rec to_rev_seq' n acc =
      match n with
      | Node {left; e; right; _} -> to_rev_seq' right @@ Seq.cons e @@ to_rev_seq' left acc
      | Leaf -> acc
    in
    to_rev_seq' tree.node Seq.empty

  let to_list tree =
    let rec to_list' n acc =
      match n with
      | Node {left; e; right; _} -> to_list' left (e :: to_list' right acc)
      | Leaf -> acc
    in
    to_list' tree.node []

  let to_rev_list tree =
    let rec to_rev_list' n acc =
      match n with
      | Node {left; e; right; _} -> to_rev_list' right (e :: to_rev_list' left acc)
      | Leaf -> acc
    in
    to_rev_list' tree.node []
end
