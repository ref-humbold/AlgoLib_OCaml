(* Structure of binomial heap *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem

  type t

  exception EmptyHeap

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

  type t = (int * bitree) list

  exception EmptyHeap

  let empty = []

  let is_empty h =
    match h with
    | [] -> true
    | _ -> false

  let rank_ (r, _) = r

  let link_ (r1, t1) (r2, t2) =
    match (t1, t2) with
    | Tree (x, ts1), Tree (y, ts2) when r1 = r2 ->
      if Cmp.compare x y < 0 then (r1 + 1, Tree (x, t2 :: ts1)) else (r1 + 1, Tree (y, t1 :: ts2))
    | Tree _, Tree _ -> failwith "unexpected"

  let rec insert_tree_ t ts =
    match ts with
    | t' :: ts' -> if rank_ t < rank_ t' then t :: ts else insert_tree_ (link_ t t') ts'
    | [] -> [t]

  let rec merge ts1 ts2 =
    match (ts1, ts2) with
    | t1' :: ts1', t2' :: ts2' ->
      if rank_ t1' < rank_ t2'
      then t1' :: merge ts1' ts2
      else if rank_ t1' > rank_ t2'
      then t2' :: merge ts1 ts2'
      else insert_tree_ (link_ t1' t2') (merge ts1' ts2')
    | ts, [] | [], ts -> ts

  let peek h =
    let rec peek' ts min =
      match (ts, min) with
      | (_, Tree (y, _)) :: ts', Some x ->
        if Cmp.compare y x <= 0 then peek' ts' @@ Some y else peek' ts' min
      | (_, Tree (y, _)) :: ts', None -> peek' ts' @@ Some y
      | [], x -> x
    in
    match peek' h None with
    | Some x -> x
    | None -> raise EmptyHeap

  let push x h = insert_tree_ (0, Tree (x, [])) h

  let pop h =
    let rec remove_min h' =
      match h' with
      | [t] -> (t, [])
      | ((_, Tree (x, _)) as t) :: ts ->
        let ((_, Tree (y, _)) as tx), tsx = remove_min ts in
        if Cmp.compare x y <= 0 then (t, ts) else (tx, t :: tsx)
      | [] -> raise EmptyHeap
    in
    let rec rank_trees rk ts acc =
      match ts with
      | t' :: ts' -> rank_trees (rk - 1) ts' ((rk, t') :: acc)
      | [] -> acc
    in
    let (r, Tree (_, ts)), h' = remove_min h in
    merge (rank_trees (r - 1) ts []) h'
end
