(* Double-ended queue structure. *)
type 'a t = 'a list * 'a list

exception EmptyDeque

let empty = ([], [])

let is_empty dq =
  match dq with
  | [], [] -> true
  | _ -> false

let front dq =
  match dq with
  | e :: _, _ | [], e :: _ -> e
  | [], [] -> raise EmptyDeque

let back dq =
  match dq with
  | _, e :: _ | e :: _, [] -> e
  | [], [] -> raise EmptyDeque

let push_front e dq =
  match dq with
  | [x], [] -> ([e], [x])
  | [x1; x2], [y] -> ([e; x1], [y; x2])
  | ft, bk -> (e :: ft, bk)

let (@++) = push_front

let push_back dq e =
  match dq with
  | [], [x] -> ([x], [e])
  | [y], [x1; x2] -> ([y; x2], [e; x1])
  | ft, bk -> (ft, e :: bk)

let (&++) = push_back

let balance_ lst =
  let rec balance_' lst' ft n =
    match lst' with
    | e :: es ->
      if n > 0
      then balance_' es (e :: ft) (n - 1)
      else (List.rev ft, List.rev lst')
    | [] -> ([], [])
  in
  balance_' lst [] @@ List.length lst / 2

let pop_front dq =
  match dq with
  | [_], bk -> balance_ bk
  | _ :: ft, bk -> (ft, bk)
  | [], _ :: bk -> ([], bk)
  | [], [] -> raise EmptyDeque

let pop_back dq =
  match dq with
  | ft, [_] -> balance_ ft
  | ft, _ :: bk -> (ft, bk)
  | _ :: ft, [] -> (ft, [])
  | [], [] -> raise EmptyDeque
