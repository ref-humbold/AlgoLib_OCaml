(* SKOŚNA LISTA INDEKSOWANA *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
type 'a t = (int * 'a tree) list

exception EmptyList
exception InvalidIndex

let empty = []

let is_empty ts =
  match ts with
  | [] -> true
  | _ -> false

let head ts =
  match ts with
  | (_, Node (_, e, _))::_ -> e
  | [] -> raise EmptyList
  | (_, Leaf)::_ -> failwith "UNEXPECTED"

let cons e ts =
  match ts with
  | (s1, t1)::(s2, t2)::ts_ when s1 = s2 ->
    (s1 + s2 + 1, Node (t1, e, t2))::ts_
  | _ -> (1, Node (Leaf, e, Leaf))::ts

let tail ts =
  match ts with
  | (1, Node _)::ts_ -> ts_
  | (s, Node (t1, _, t2))::ts_ -> (s / 2, t1)::(s / 2, t2)::ts_
  | [] -> raise EmptyList
  | (_, Leaf)::_ -> failwith "UNEXPECTED"

let rec elem i ts =
  let rec elemTree ix s t =
    match (ix, t) with
    | (0, Node (_, e, _)) -> e
    | (_, Node (t1, _, t2)) ->
      if 2 * ix < s
      then elemTree ((s - 1) / 2) (ix - 1) t1
      else elemTree ((s - 1) / 2) (ix - (s + 1) / 2) t2
    | (_, Leaf) -> failwith "UNEXPECTED" in
  match ts with
  | (s, t)::ts_ -> if i < s then elemTree i s t else elem (i - s) ts_
  | [] -> raise InvalidIndex

let rec update i e ts =
  let rec updateTree ix s t =
    match (ix, t) with
    | (0, Node (t1, _, t2)) -> (s, Node (t1, e, t2))
    | (_, Node (t1, _, t2)) ->
      if 2 * ix < s
      then updateTree ((s - 1) / 2) (ix - 1) t1
      else updateTree ((s - 1) / 2) (ix - (s + 1) / 2) t2
    | (_, Leaf) -> failwith "UNEXPECTED" in
  match ts with
  | (s, t)::ts_ -> if i < s then (updateTree i s t)::ts_ else update (i - s) e ts_
  | [] -> raise InvalidIndex
