(* DOUBLE-ENDED QUEUE STRUCTURE *)
type 'a t = 'a list * 'a list

exception EmptyDeque

let create () = ([], [])

let is_empty dq =
  match dq with
  | ([], []) -> true
  | _ -> false

let front dq =
  match dq with
  | (e::_, _) -> e
  | ([], e::_) -> e
  | ([], []) -> raise EmptyDeque

let back dq =
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

let balance_ d =
  let rec balance_' d' n =
    match d' with
    | e::ds ->
      if n > 0
      then let (nft, nbk) = balance_' ds (n - 1) in (nft, e::nbk)
      else (List.rev d', [])
    | [] -> ([], []) in
  balance_' d @@ (List.length d) / 2

let pop_front dq =
  match dq with
  | ([_], bk) -> balance_ bk
  | (_::ft, bk) -> (ft, bk)
  | ([], _::bk) -> ([], bk)
  | ([], []) -> raise EmptyDeque

let pop_back dq =
  match dq with
  | (ft, [_]) -> let (nbk, nft) = balance_ ft in (nft, nbk)
  | (ft, _::bk) -> (ft, bk)
  | (_::ft, []) -> (ft, [])
  | ([], []) -> raise EmptyDeque