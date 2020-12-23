(* Structure of red-black tree *)
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

  val to_seq : t -> elem Seq.t

  val contains : elem -> t -> bool

  val add : elem -> t -> t
end

module Make (Cmp : COMPARABLE) : RBTREE with type elem = Cmp.t = struct
  type elem = Cmp.t

  type colour = Black | Red

  type tree = Leaf | Node of {cl : colour; lt : tree; e : elem; rt : tree}

  type t = {size : int; t : tree}

  let empty = {size = 0; t = Leaf}

  let is_empty {size; _} = size > 0

  let size {size; _} = size

  let to_list {t; _} =
    let rec to_list' t' acc =
      match t' with
      | Node {lt; e; rt; _} -> to_list' lt (e :: to_list' rt acc)
      | Leaf -> acc
    in
    to_list' t []

  let to_seq {t; _} =
    let rec to_seq' t' acc =
      match t' with
      | Node {lt; e; rt; _} -> to_seq' lt @@ Seq.Cons (e, to_seq' rt acc)
      | Leaf -> fun () -> acc
    in
    to_seq' t Seq.Nil

  let contains x {t; _} =
    let rec contains' t' =
      match t' with
      | Node {lt; e; rt; _} ->
        let cond = Cmp.compare x e in
        if cond = 0 then true else if cond < 0 then contains' lt else contains' rt
      | Leaf -> false
    in
    contains' t

  let rebalance_ cl lt e rt =
    match (cl, lt, e, rt) with
    | Black, Node {cl = Red; lt = Node {cl = Red; lt = a; e = x; rt = b}; e = y; rt = c}, z, d ->
      Node
        { cl = Red;
          lt = Node {cl = Black; lt = a; e = x; rt = b};
          e = y;
          rt = Node {cl = Black; lt = c; e = z; rt = d} }
    | Black, Node {cl = Red; lt = a; e = x; rt = Node {cl = Red; lt = b; e = y; rt = c}}, z, d ->
      Node
        { cl = Red;
          lt = Node {cl = Black; lt = a; e = x; rt = b};
          e = y;
          rt = Node {cl = Black; lt = c; e = z; rt = d} }
    | Black, a, x, Node {cl = Red; lt = b; e = y; rt = Node {cl = Red; lt = c; e = z; rt = d}} ->
      Node
        { cl = Red;
          lt = Node {cl = Black; lt = a; e = x; rt = b};
          e = y;
          rt = Node {cl = Black; lt = c; e = z; rt = d} }
    | Black, a, x, Node {cl = Red; lt = Node {cl = Red; lt = b; e = y; rt = c}; e = z; rt = d} ->
      Node
        { cl = Red;
          lt = Node {cl = Black; lt = a; e = x; rt = b};
          e = y;
          rt = Node {cl = Black; lt = c; e = z; rt = d} }
    | Black, Node _, _, Node _
    |Black, Node _, _, Leaf
    |Black, Leaf, _, Node _
    |Black, Leaf, _, Leaf
    |Red, Node _, _, Node _
    |Red, Node _, _, Leaf
    |Red, Leaf, _, Node _
    |Red, Leaf, _, Leaf ->
      Node {cl; lt; e; rt}

  let add x ({size; t} as s) =
    let rec add' t' =
      match t' with
      | Node {cl; lt; e; rt} ->
        let cond = Cmp.compare x e in
        if cond = 0
        then None
        else if cond < 0
        then
          match add' lt with
          | Some lt' -> Some (rebalance_ cl lt' e rt)
          | None -> None
        else (
          match add' rt with
          | Some rt' -> Some (rebalance_ cl lt e rt')
          | None -> None )
      | Leaf -> Some (Node {cl = Red; lt = Leaf; e = x; rt = Leaf})
    in
    match add' t with
    | Some (Node nd) -> {size = size + 1; t = Node {nd with cl = Black}}
    | None -> s
    | Some Leaf -> failwith "unexpected"
end
