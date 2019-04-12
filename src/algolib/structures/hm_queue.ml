(* Real-time Hood-Melville queue structure *)
type 'a combining = Rev of int * 'a list * 'a list * 'a list * 'a list
                  | Apd of int * 'a list * 'a list
                  | End of 'a list
                  | Null
type 'a t = int * 'a list * 'a combining * int * 'a list

exception EmptyQueue

let create () = (0, [], Null, 0, [])

let is_empty (flen, ft, _, _, bk) =
  match flen, ft, bk with
  | 0, [], [] -> true
  | _ -> false

let front (_, ft, _, _, _) =
  match ft with
  | x :: _ -> x
  | [] -> raise EmptyQueue

let rebalance_ ((flen, ft, _, blen, bk) as q) =
  let comb rs =
    match rs with
    | Rev (len, x :: ft', rft, y :: bk', rbk) -> Rev (len, ft', x :: rft, bk', y :: rbk)
    | Rev (len, [], ft', [y], bk') -> Apd (len, ft', y :: bk')
    | Apd (0, _, bk') -> End bk'
    | Apd (len, x :: ft', bk') -> Apd (len, ft', x :: bk')
    | Rev _ | Apd _ | End _ | Null -> rs in
  let combine (flen', ft', rs', blen', bk') =
    match comb (comb rs') with
    | End ft'' -> (flen', ft'', Null, blen', bk')
    | (Rev _ as rs'') | (Apd _ as rs'') | (Null as rs'') ->
      (flen', ft', rs'', blen', bk') in
  if blen <= flen
  then combine q
  else let rev = Rev (0, ft, [], bk, []) in combine (flen + blen, ft, rev, 0, [])

let push x (flen, ft, rs, blen, bk) = rebalance_ (flen, ft, rs, blen + 1, x :: bk)

let pop (flen, ft, rs, blen, bk) =
  let decrement rs =
    match rs with
    | Rev (len, ft, rft, bk, rbk) -> Rev (len - 1, ft, rft, bk, rbk)
    | Apd (0, _, _ :: bk) -> End bk
    | Apd (len, ft, bk) -> Apd (len - 1, ft, bk)
    | End _ | Null -> rs in
  match ft with
  | _ :: xs -> rebalance_ (flen - 1, xs, decrement rs, blen, bk)
  | [] -> raise EmptyQueue
