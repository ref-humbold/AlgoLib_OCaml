(* LISTA INDEKSOWANA *)
type 'a tree = Leaf of 'a | Node of int * 'a tree * 'a tree
type 'a count = One of 'a tree | Two of 'a tree * 'a tree
type 'a t = 'a count list

exception EmptyList
exception InvalidIndex

let empty = []

let is_empty ts =
  match ts with
  | [] -> true
  | _ -> false

let size t =
  match t with
  | Leaf _ -> 1
  | Node (s, _, _) -> s

let link t1 t2 = Node (size t1 + size t2, t1, t2)

let rec unconsTree t =
  match t with
  | [One t] -> (t, [])
  | (Two (t1, t2))::ts -> (t1, (One t2)::ts)
  | (One t)::ts ->
    (match unconsTree ts with
     | (Node (_, t1, t2), nts) -> (t, (Two (t1, t2))::nts)
     | (Leaf _, _) -> failwith "UNEXPECTED")
  | [] -> raise EmptyList

let head ts =
  match unconsTree ts with
  | (Leaf e, _) -> e
  | (Node _, _) -> failwith "UNEXPECTED"

let cons e ts =
  let rec consTree t trs =
    match trs with
    | (One tx)::txs -> (Two (t, tx))::txs
    | (Two (tx1, tx2))::txs -> (One t)::(consTree (link tx1 tx2) txs)
    | [] -> [One t] in
  consTree (Leaf e) ts

let tail ts = let (_, nts) = unconsTree ts in nts

let rec elem i ts =
  let rec elemTree ix t =
    match t with
    | Leaf e -> if ix = 0 then e else raise InvalidIndex
    | Node (s, t1, t2) ->
      if ix < s / 2
      then elemTree ix t1
      else elemTree (ix - s / 2) t2 in
  match ts with
  | (One t)::ts_ ->
    if i < size t
    then elemTree i t
    else elem (i - size t) ts_
  | (Two (t1, t2))::ts_ ->
    if i < size t1 + size t2
    then elemTree i @@ link t1 t2
    else elem (i - size t1 - size t2) ts_
  | [] -> raise InvalidIndex

let rec update i e ts =
  let rec updateTree ix t =
    match t with
    | Leaf _ -> if ix = 0 then Leaf e else raise InvalidIndex
    | Node (s, t1, t2) ->
      if ix < s / 2
      then Node (s, updateTree ix t1, t2)
      else Node (s, t1, updateTree (i - s / 2) t2) in
  match ts with
  | ((One tx) as t)::ts_ ->
    if i < size tx
    then (One (updateTree i tx))::ts_
    else t::(update (i - size tx) e ts_)
  | ((Two (t1, t2)) as t)::ts_ ->
    if i < size t1
    then Two (updateTree i t1, t2)::ts_
    else if i - size t1 < size t2
    then Two (t1, updateTree (i - size t1) t2)::ts_
    else t::(update (i - size t1 - size t2) e ts_)
  | [] -> raise InvalidIndex
