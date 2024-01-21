(* Structure of binomial heap. *)
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

  val length : t -> int

  val merge : t -> t -> t

  val peek : t -> elem

  val peek_opt : t -> elem option

  val push : elem -> t -> t

  val pop : t -> t

  val of_seq : elem Seq.t -> t

  val of_list : elem list -> t
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t = struct
  type elem = Cmp.t

  type bitree = Tree of elem * bitree list

  type node = {rank : int; tree : bitree}

  type t = {size : int; nodes : node list}

  exception Empty_heap

  let empty = {size = 0; nodes = []}

  let is_empty heap =
    match heap.nodes with
    | [] -> true
    | _ -> false

  let length heap = heap.size

  let link_ node1 node2 =
    match (node1.tree, node2.tree) with
    | Tree (e1, ts1), Tree (e2, ts2) when node1.rank = node2.rank ->
      if Cmp.compare e1 e2 < 0
      then {rank = node1.rank + 1; tree = Tree (e1, node2.tree :: ts1)}
      else {rank = node1.rank + 1; tree = Tree (e2, node1.tree :: ts2)}
    | Tree _, Tree _ -> failwith "unexpected"

  let rec insert_node_ node nodes =
    match nodes with
    | n :: ns -> if node.rank < n.rank then node :: nodes else insert_node_ (link_ node n) ns
    | [] -> [node]

  let rec merge_ nodes1 nodes2 =
    match (nodes1, nodes2) with
    | n1 :: ns1, n2 :: ns2 ->
      if n1.rank < n2.rank
      then n1 :: merge_ ns1 nodes2
      else if n1.rank > n2.rank
      then n2 :: merge_ nodes1 ns2
      else insert_node_ (link_ n1 n2) (merge_ ns1 ns2)
    | ts, [] | [], ts -> ts

  let merge {size = size1; nodes = nodes1} {size = size2; nodes = nodes2} =
    {size = size1 + size2; nodes = merge_ nodes1 nodes2}

  let peek_opt heap =
    let rec peek' ns min =
      match (ns, min) with
      | {tree = Tree (e, _); _} :: ns', Some min' ->
        if Cmp.compare e min' <= 0 then peek' ns' @@ Some e else peek' ns' min
      | {tree = Tree (e, _); _} :: ns', None -> peek' ns' @@ Some e
      | [], x -> x
    in
    peek' heap.nodes None

  let peek heap =
    match peek_opt heap with
    | Some e -> e
    | None -> raise Empty_heap

  let push element {size; nodes} =
    {size = size + 1; nodes = insert_node_ {rank = 0; tree = Tree (element, [])} nodes}

  let pop {size; nodes} =
    let rec remove_min ns =
      match ns with
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
    let {rank; tree = Tree (_, ts)}, nodes' = remove_min nodes in
    {size = size - 1; nodes = merge_ (rank_trees (rank - 1) ts []) nodes'}

  let of_seq xs = Seq.fold_left (fun acc e -> push e acc) empty xs

  let of_list xs = List.fold_left (fun acc e -> push e acc) empty xs
end
