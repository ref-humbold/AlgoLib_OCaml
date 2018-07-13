(** BINOMIAL HEAP STRUCTURE *)
module type COMPARABLE =
sig
  type c = Less | Equal | Greater
  type t
  val cmp: t -> t -> c
end

module type HEAP =
sig
  type elem
  type t
  exception EmptyHeap
  val empty: t
  val is_empty: t -> bool
  val merge: t -> t -> t
  val peek: t -> elem
  val push: elem -> t -> t
  val pop: t -> t
end

module Make(Cmp: COMPARABLE) =
struct
  type elem = Cmp.t
  type bitree = Tree of elem * bitree list
  type t = (int * bitree) list

  exception EmptyHeap

  let empty = []

  let is_empty h =
    match h with
    | _::_ -> false
    | [] -> true

  let rank' (r, _) = r

  let link' (r1, t1) (r2, t2) =
    match (t1, t2) with
    | (Tree (x, ts1), Tree (y, ts2)) when r1 = r2 ->
      if x < y
      then (r1 + 1, Tree (x, t2::ts1))
      else (r1 + 1, Tree (y, t1::ts2))
    | (Tree _, Tree _) -> failwith "UNEXPECTED"

  let rec insert_tree' t ts =
    match ts with
    | tx::tsx ->
      if rank' t < rank' tx
      then t::ts
      else insert_tree' (link' t tx) tsx
    | [] -> [t]

  let rec merge ts1 ts2 =
    match (ts1, ts2) with
    | (tx1::tsx1, tx2::tsx2) ->
      if rank' tx1 < rank' tx2
      then tx1::(merge tsx1 ts2)
      else if rank' tx1 > rank' tx2
      then tx2::(merge ts1 tsx2)
      else insert_tree' (link' tx1 tx2) (merge tsx1 tsx2)
    | (ts, []) | ([], ts) -> ts

  let peek h =
    let rec peek_ ts min =
      match (ts, min) with
      | ((_, Tree (y, _))::tsx, Some x) ->
        if y <= x
        then peek_ tsx @@ Some y
        else peek_ tsx min
      | ((_, Tree (y, _))::tsx, None) -> peek_ tsx @@ Some y
      | ([], x) -> x in
    match peek_ h None with
    | Some x -> x
    | None -> raise EmptyHeap

  let push x h = insert_tree' (0, Tree (x, [])) h

  let pop h =
    let rec remove_min h_ =
      match h_ with
      | [t] -> (t, [])
      | ((_, Tree (x, _)) as t)::ts ->
        (match remove_min ts with
         | ((_, Tree (y, _)) as tx, tsx) ->
           if x <= y
           then (t, ts)
           else (tx, t::tsx))
      | [] -> raise EmptyHeap in
    let rec rank_trees rk ts acc =
      match ts with
      | tx::tsx -> rank_trees (rk - 1) tsx @@ (rk, tx)::acc
      | [] -> acc in
    match remove_min h with
    | ((r, Tree (_, ts)), hx) -> merge (rank_trees (r - 1) ts []) hx
end
