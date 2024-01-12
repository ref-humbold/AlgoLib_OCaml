(* Structure of binomial heap *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem

  type t

  exception Empty_heap

  val empty : t

  val is_empty : t -> bool

  val merge : t -> t -> t

  val peek : t -> elem

  val push : elem -> t -> t

  val pop : t -> t
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t = struct
  type elem = Cmp.t

  type bitree = Tree of elem * bitree list

  type node = {rank : int; tree : bitree}

  type t = node list

  exception Empty_heap

  let empty = []

  let is_empty heap =
    match heap with
    | [] -> true
    | _ -> false

  let link_ node1 node2 =
    match (node1.tree, node2.tree) with
    | Tree (e1, ts1), Tree (e2, ts2) when node1.rank = node2.rank ->
      if Cmp.compare e1 e2 < 0
      then {rank = node1.rank + 1; tree = Tree (e1, node2.tree :: ts1)}
      else {rank = node1.rank + 1; tree = Tree (e2, node1.tree :: ts2)}
    | Tree _, Tree _ -> failwith "unexpected"

  let rec insert_tree_ t ts =
    match ts with
    | t' :: ts' -> if t.rank < t.rank then t :: ts else insert_tree_ (link_ t t') ts'
    | [] -> [t]

  let rec merge heap1 heap2 =
    match (heap1, heap2) with
    | t1 :: ts1, t2 :: ts2 ->
      if t1.rank < t2.rank
      then t1 :: merge ts1 heap2
      else if t1.rank > t2.rank
      then t2 :: merge heap1 ts2
      else insert_tree_ (link_ t1 t2) (merge ts1 ts2)
    | ts, [] | [], ts -> ts

  let peek heap =
    let rec peek' ts min =
      match (ts, min) with
      | {tree = Tree (e, _); _} :: ts', Some min' ->
        if Cmp.compare e min' <= 0 then peek' ts' @@ Some e else peek' ts' min
      | {tree = Tree (e, _); _} :: ts', None -> peek' ts' @@ Some e
      | [], x -> x
    in
    match peek' heap None with
    | Some e -> e
    | None -> raise Empty_heap

  let push element heap = insert_tree_ {rank = 0; tree = Tree (element, [])} heap

  let pop heap =
    let rec remove_min ts =
      match ts with
      | [t] -> (t, [])
      | ({tree = Tree (e, _); _} as t) :: ts' ->
        let ({tree = Tree (e', _); _} as t'), ts'' = remove_min ts' in
        if Cmp.compare e e' <= 0 then (t, ts') else (t', t :: ts'')
      | [] -> raise Empty_heap
    in
    let rec rank_trees rank ts acc =
      match ts with
      | tree :: ts' -> rank_trees (rank - 1) ts' ({rank; tree} :: acc)
      | [] -> acc
    in
    let {rank; tree = Tree (_, ts)}, h' = remove_min heap in
    merge (rank_trees (rank - 1) ts []) h'
end
