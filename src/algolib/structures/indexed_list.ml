(* Structure of indexed list *)
type 'a tree = Leaf | Node of {left : 'a tree; e : 'a; right : 'a tree}

type 'a item = {size : int; tree : 'a tree}

type 'a t = 'a item list

exception Empty_list

exception Invalid_index

let empty = []

let is_empty list =
  match list with
  | [] -> true
  | _ -> false

let length list =
  let rec length' list' acc =
    match list' with
    | {size; _} :: is -> length' is (acc + size)
    | [] -> acc
  in
  length' list 0

let head list =
  match list with
  | {tree = Node {e; _}; _} :: _ -> e
  | [] -> raise Empty_list
  | {tree = Leaf; _} :: _ -> failwith "unexpected"

let cons e list =
  match list with
  | {size = s1; tree = left} :: {size = s2; tree = right} :: is when s1 = s2 ->
    {size = s1 + s2 + 1; tree = Node {left; e; right}} :: is
  | _ -> {size = 1; tree = Node {left = Leaf; e; right = Leaf}} :: list

let ( @:: ) = cons

let tail list =
  match list with
  | {size = 1; tree = Node _} :: is -> is
  | {size; tree = Node {left; right; _}} :: is ->
    {size = size / 2; tree = left} :: {size = size / 2; tree = right} :: is
  | [] -> raise Empty_list
  | {tree = Leaf; _} :: _ -> failwith "unexpected"

let rec get i list =
  let rec get' i' size tree =
    match (i', tree) with
    | 0, Node {e; _} -> e
    | _, Node {left; right; _} ->
      if 2 * i' < size
      then get' ((size - 1) / 2) (i' - 1) left
      else get' ((size - 1) / 2) (i' - ((size + 1) / 2)) right
    | _, Leaf -> failwith "unexpected"
  in
  match list with
  | {size; tree} :: items when i >= 0 -> if i < size then get' i size tree else get (i - size) items
  | _ -> raise Invalid_index

let ( &! ) list index = get index list

let rec set i e list =
  let rec set' i' size tree =
    match (i', tree) with
    | 0, Node {left; right; _} -> {size; tree = Node {left; e; right}}
    | _, Node {left; right; _} ->
      if 2 * i' < size
      then set' ((size - 1) / 2) (i' - 1) left
      else set' ((size - 1) / 2) (i' - ((size + 1) / 2)) right
    | _, Leaf -> failwith "unexpected"
  in
  match list with
  | {size; tree} :: items when i >= 0 ->
    if i < size then set' i size tree :: items else set (i - size) e items
  | _ -> raise Invalid_index

let rec of_seq xs =
  match Seq.uncons xs with
  | Some (x, xs') -> cons x @@ of_seq xs'
  | None -> empty

let of_list xs = List.fold_right cons xs empty

let to_seq list =
  let rec tree_seq tree acc =
    match tree with
    | Node {left; e; right} -> tree_seq left @@ Seq.cons e @@ tree_seq right acc
    | Leaf -> acc
  in
  let rec to_seq' list' acc =
    match list' with
    | {tree; _} :: items -> to_seq' items @@ tree_seq tree acc
    | [] -> acc
  in
  to_seq' list Seq.empty

let to_list list =
  let rec tree_list tree acc =
    match tree with
    | Node {left; e; right} -> tree_list left (e :: tree_list right acc)
    | Leaf -> acc
  in
  let rec to_list' list' acc =
    match list' with
    | {tree; _} :: items -> to_list' items @@ tree_list tree acc
    | [] -> acc
  in
  to_list' list []
