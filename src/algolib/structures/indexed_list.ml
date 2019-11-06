(* Indexed list structure. *)
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
  | (_, Node (_, e, _)) :: _ -> e
  | [] -> raise EmptyList
  | (_, Leaf) :: _ -> failwith "UNEXPECTED"

let to_list ts =
  let rec tree_list t acc =
    match t with
    | Node (lt, x, rt) -> tree_list lt (x :: tree_list rt acc)
    | Leaf -> acc
  in
  let rec to_list' ts' acc =
    match ts' with
    | (_, t) :: ts_ -> to_list' ts_ @@ tree_list t acc
    | [] -> acc
  in
  to_list' ts []

let to_seq ts =
  let rec tree_seq t acc =
    match t with
    | Node (lt, x, rt) -> tree_seq lt @@ Seq.Cons (x, tree_seq rt acc)
    | Leaf -> fun () -> acc
  in
  let rec to_seq' ts' acc =
    match ts' with
    | (_, t) :: ts_ -> to_seq' ts_ @@ tree_seq t acc ()
    | [] -> fun () -> acc
  in
  to_seq' ts Seq.Nil

let cons e ts =
  match ts with
  | (s1, t1) :: (s2, t2) :: ts_ when s1 = s2 -> (s1 + s2 + 1, Node (t1, e, t2)) :: ts_
  | _ -> (1, Node (Leaf, e, Leaf)) :: ts

let tail ts =
  match ts with
  | (1, Node _) :: ts_ -> ts_
  | (s, Node (t1, _, t2)) :: ts_ -> (s / 2, t1) :: (s / 2, t2) :: ts_
  | [] -> raise EmptyList
  | (_, Leaf) :: _ -> failwith "UNEXPECTED"

let rec elem i ts =
  let rec elem' ix s t =
    match (ix, t) with
    | 0, Node (_, e, _) -> e
    | _, Node (t1, _, t2) ->
      if 2 * ix < s
      then elem' ((s - 1) / 2) (ix - 1) t1
      else elem' ((s - 1) / 2) (ix - ((s + 1) / 2)) t2
    | _, Leaf -> failwith "UNEXPECTED"
  in
  match ts with
  | (s, t) :: ts_ when i >= 0 -> if i < s then elem' i s t else elem (i - s) ts_
  | _ -> raise InvalidIndex

let rec update i e ts =
  let rec update' ix s t =
    match (ix, t) with
    | 0, Node (t1, _, t2) -> (s, Node (t1, e, t2))
    | _, Node (t1, _, t2) ->
      if 2 * ix < s
      then update' ((s - 1) / 2) (ix - 1) t1
      else update' ((s - 1) / 2) (ix - ((s + 1) / 2)) t2
    | _, Leaf -> failwith "UNEXPECTED"
  in
  match ts with
  | (s, t) :: ts_ when i >= 0 -> if i < s then update' i s t :: ts_ else update (i - s) e ts_
  | _ -> raise InvalidIndex
