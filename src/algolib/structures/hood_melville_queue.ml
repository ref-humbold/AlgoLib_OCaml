(* REAL-TIME HOOD-MELVILLE QUEUE STRUCTURE *)
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
  | x::_ -> x
  | [] -> raise EmptyQueue

let rebalance_ ((flen, ft, _, blen, bk) as q) =
  let comb rs =
    match rs with
    | Rev (len, x::ft_, rft, y::bk_, rbk) -> Rev (len, ft_, x::rft, bk_, y::rbk)
    | Rev (len, [], rft, [y], rbk) -> Apd (len, rft, y::rbk)
    | Apd (0, _, bk_) -> End bk_
    | Apd (len, x::ft_, bk_) -> Apd (len, ft_, x::bk_)
    | Rev _ | Apd _ | End _ | Null -> rs in
  let combine (flen_, ft_, rs, blen_, bk_) =
    match comb (comb rs) with
    | End nft -> (flen_, nft, Null, blen_, bk_)
    | (Rev _ as nrs) | (Apd _ as nrs) | (Null as nrs) ->
      (flen_, ft_, nrs, blen_, bk_) in
  if blen <= flen
  then combine q
  else let rev = Rev (0, ft, [], bk, []) in combine (flen + blen, ft, rev, 0, [])

let push x (flen, ft, rs, blen, bk) = rebalance_ (flen, ft, rs, blen + 1, x::bk)

let pop (flen, ft, rs, blen, bk) =
  let decrement rs =
    match rs with
    | Rev (len, ft, rft, bk, rbk) -> Rev (len - 1, ft, rft, bk, rbk)
    | Apd (0, _, _::bk) -> End bk
    | Apd (len, ft, bk) -> Apd (len -1, ft, bk)
    | End _ | Null -> rs in
  match ft with
  | _::xs -> rebalance_ (flen - 1, xs, decrement rs, blen, bk)
  | [] -> raise EmptyQueue
