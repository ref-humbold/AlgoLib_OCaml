(* KOLEJKA DWUKOŃCOWA *)
type 'a deque = 'a list * 'a list

exception EmptyDeque

let create () = ([], [])

let is_empty dq =
  match dq with
  | ([], []) -> true
  | _ -> false

let get_front dq =
  match dq with
  | (e::_, _) -> e
  | ([], e::_) -> e
  | ([], []) -> raise EmptyDeque

let get_back dq =
  match dq with
  | (_, e::_) -> e
  | (e::_, []) -> e
  | ([], []) -> raise EmptyDeque

let push_front e dq =
  match dq with
  | ([x], []) -> ([e], [x])
  | ([x1; x2], [y]) -> ([e; x1], [y; x2])
  | (ft, bk) -> (e::ft, bk)

let push_back e dq =
  match dq with
  | ([], [x]) -> ([x], [e])
  | ([y], [x1; x2]) -> ([y; x2], [e; x1])
  | (ft, bk) -> (ft, e::bk)

let balance d =
  let rec balance_ d_ n_ =
    match d_ with
    | e::ds ->
      if n_ > 0
      then let (nft, nbk) = balance_ ds (n_ - 1) in (nft, e::nbk)
      else (List.rev d_, [])
    | [] -> ([], []) in
  balance_ d ((List.length d) / 2)

let pop_front dq =
  match dq with
  | ([e], bk) -> balance bk
  | (e::ft, bk) -> (ft, bk)
  | ([], e::bk) -> ([], bk)
  | ([], []) -> raise EmptyDeque

let pop_back dq =
  match dq with
  | (ft, [e]) -> let (nbk, nft) = balance ft in (nft, nbk)
  | (ft, e::bk) -> (ft, bk)
  | (e::ft, []) -> (ft, [])
  | ([], []) -> raise EmptyDeque
