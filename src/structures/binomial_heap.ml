(* KOPIEC DWUMIANOWY *)
type 'a tree = Tree of int * 'a * 'a tree list
type 'a heap = 'a tree list

exception EmptyHeap

let create () = []

let is_empty h =
  match h with
  | _::_ -> false
  | [] -> true

let rank t =
  match t with
  | Tree (r, _, _) -> r

let link t1 t2 =
  match (t1, t2) with
  | (Tree (r1, x, ts1), Tree (r2, y, ts2)) when r1 = r2 ->
    if x < y
    then Tree (r1 + 1, x, t2::ts1)
    else Tree (r1 + 1, y, t1::ts2)
  | _ -> -> raise Failure

let rec insert_tree t ts =
  match ts with
  | tx::tsx ->
    if rank t < rank tx
    then t::ts
    else insert_tree (link t tx) tsx
  | [] -> [t]

let push x h = insert_tree (Tree (0, x, [])) h

let rec merge ts1 ts2 =
  match (ts1, ts2) with
  | (tx1::tsx1, tx2::tsx2) ->
    if rank tx1 < rank tx2
    then tx1::(merge tsx1 ts2)
    else if rank tx1 > rank tx2
    then tx2::(merge ts1 tsx2)
    else insert_tree @@ link tx1 tx2 @@ merge tsx1 tsx2
  | (ts, []) | ([], ts) -> ts

let pop h =
  let rec remove_min h_ =
    match h_ with
    | [t] -> (t, [])
    | (Tree (_, x, _) as t)::ts ->
      (match remove_min ts with
       | (Tree (_, y, _) as tx, tsx) ->
         if x <= y
         then (t, ts)
         else (tx, t::tsx))
    | [] -> raise EmptyHeap in
  match remove_min h with
  | (Tree (_, _, ts), hx) -> merge (List.rev ts) hx

let get h =
  let rec get_ ts min =
    match (ts, min) with
    | ((Tree (_, y, _))::tsx, Some x) ->
      if y <= x
      then get_ tsx @@ Some y
      else get_ tsx min
    | ((Tree (_, y, _))::tsx, None) -> get_ tsx @@ Some y
    | ([], x) -> x in
  match get_ h None with
  | Some x -> x
  | None -> raise EmptyHeap
