(* KOPIEC DWUMIANOWY *)
type 'a tree = Tree of 'a * 'a tree list;;
type 'a heap = int * 'a tree list;;

exception EmptyHeap;;

let create () = [];;

let is_empty h =
  match h with
  | _::_ -> false
  | [] -> true;;

let rank (r, _) = r;;

let link t1 t2 =
  match (t1, t2) with
  | ((r1, (Tree (x, ts1) as tx1)), (r2, (Tree (y, ts2) as tx2)))
    when r1 = r2 ->
    if x < y
    then (r1 + 1, Tree (x, tx2::ts1))
    else (r1 + 1, Tree (y, tx1::ts2))
  | _ -> failwith "unexpected";;

let rec insert_tree t ts =
  match ts with
  | tx::tsx ->
    if rank t < rank tx
    then t::ts
    else insert_tree (link t tx) tsx
  | [] -> [t];;

let push x h = insert_tree (0, Tree (x, [])) h;;

let rec merge ts1 ts2 =
  match (ts1, ts2) with
  | (tx1::tsx1, tx2::tsx2) ->
    if rank tx1 < rank tx2
    then tx1::(merge tsx1 ts2)
    else if rank tx1 > rank tx2
    then tx2::(merge ts1 tsx2)
    else insert_tree (link tx1 tx2) (merge tsx1 tsx2)
  | (ts, []) | ([], ts) -> ts;;

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
  let rec rankTrees ts acc =
    match ts with
    | (Tree (_, tsx) as t)::tsy -> rankTrees tsy @@ (List.length tsx, t)::acc
    | [] -> acc in
  match remove_min h with
  | ((_, Tree (_, ts)), hx) -> merge (rankTrees ts []) hx;;

let get h =
  let rec get_ ts min =
    match (ts, min) with
    | ((_, Tree (y, _))::tsx, Some x) ->
      if y <= x
      then get_ tsx @@ Some y
      else get_ tsx min
    | ((_, Tree (y, _))::tsx, None) -> get_ tsx @@ Some y
    | ([], x) -> x in
  match get_ h None with
  | Some x -> x
  | None -> raise EmptyHeap;;
