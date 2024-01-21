(* Structure of indexed list. *)
type 'a tree = Leaf | Node of {left : 'a tree; element : 'a; right : 'a tree}

type 'a item = {size : int; tree : 'a tree}

type 'a t = 'a item list

exception Empty_list

exception Invalid_index of int

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
  | {tree = Node {element; _}; _} :: _ -> element
  | [] -> raise Empty_list
  | {tree = Leaf; _} :: _ -> failwith "unexpected"

let tail list =
  match list with
  | {size = 1; tree = Node _} :: is -> is
  | {size; tree = Node {left; right; _}} :: is ->
    {size = size / 2; tree = left} :: {size = size / 2; tree = right} :: is
  | [] -> raise Empty_list
  | {tree = Leaf; _} :: _ -> failwith "unexpected"

let cons e list =
  match list with
  | {size = s1; tree = left} :: {size = s2; tree = right} :: is when s1 = s2 ->
    {size = s1 + s2 + 1; tree = Node {left; element = e; right}} :: is
  | _ -> {size = 1; tree = Node {left = Leaf; element = e; right = Leaf}} :: list

let ( @:: ) = cons

let get i list =
  let rec get_tree i' size tree =
    match (i', tree) with
    | 0, Node {element; _} -> element
    | _, Node {left; right; _} ->
      let subsize = size / 2 in
      if 2 * i' < size
      then get_tree (i' - 1) subsize left
      else get_tree (i' - subsize - 1) subsize right
    | _, Leaf -> failwith "unexpected"
  in
  let rec get' i' list' =
    match list' with
    | {size; tree} :: items when i' >= 0 ->
      if i' < size then get_tree i' size tree else get' (i' - size) items
    | _ -> raise @@ Invalid_index i
  in
  get' i list

let ( &! ) list index = get index list

let set i e list =
  let rec set_tree i' size tree =
    match (i', tree) with
    | 0, Node {left; right; _} -> Node {left; element = e; right}
    | _, Node ({left; right; _} as n) ->
      let subsize = size / 2 in
      if 2 * i' < size
      then Node {n with left = set_tree (i' - 1) subsize left}
      else Node {n with right = set_tree (i' - subsize - 1) subsize right}
    | _, Leaf -> failwith "unexpected"
  in
  let rec set' i' list' =
    match list' with
    | ({size; tree} as item) :: items when i' >= 0 ->
      if i' < size
      then {size; tree = set_tree i' size tree} :: items
      else item :: set' (i' - size) items
    | _ -> raise @@ Invalid_index i
  in
  set' i list

let rec of_seq xs =
  match Seq.uncons xs with
  | Some (x, xs') -> cons x @@ of_seq xs'
  | None -> empty

let of_list xs = List.fold_right cons xs empty

let rec to_seq list =
  let rec tree_seq tree acc =
    match tree with
    | Node {left; element; right} -> Seq.cons element @@ tree_seq left @@ tree_seq right acc
    | Leaf -> acc
  in
  match list with
  | {tree; _} :: items -> tree_seq tree @@ to_seq items
  | [] -> Seq.empty

let to_list list =
  let rec tree_list tree acc =
    match tree with
    | Node {left; element; right} -> tree_list right @@ tree_list left (element :: acc)
    | Leaf -> acc
  in
  let rec to_list' list' acc =
    match list' with
    | {tree; _} :: items -> to_list' items @@ tree_list tree acc
    | [] -> acc
  in
  List.rev @@ to_list' list []
