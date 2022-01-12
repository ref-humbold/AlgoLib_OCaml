(* Structure of real-time Hood-Melville queue *)
type 'a rotation =
  | Reverse of {len : int; front : 'a list; rev_front : 'a list; back : 'a list; rev_back : 'a list}
  | Append of {len : int; front : 'a list; back : 'a list}
  | End of 'a list
  | Null

type 'a t = {front : int * 'a list; rotation : 'a rotation; back : int * 'a list}

exception Empty_queue

let empty = {front = (0, []); rotation = Null; back = (0, [])}

let is_empty {front = _, ft; back = _, bk; _} =
  match (ft, bk) with
  | [], [] -> true
  | _ -> false

let front {front = _, ft; _} =
  match ft with
  | x :: _ -> x
  | [] -> raise Empty_queue

let rebalance_ ({front = flen, ft; back = blen, bk; _} as q) =
  let comb rotation =
    match rotation with
    | Reverse {len; front = x :: ft'; rev_front; back = y :: bk'; rev_back} ->
      Reverse {len; front = ft'; rev_front = x :: rev_front; back = bk'; rev_back = y :: rev_back}
    | Reverse {len; front = []; rev_front; back = [y]; rev_back} ->
      Append {len; front = rev_front; back = y :: rev_back}
    | Append {len = 0; back; _} -> End back
    | Append {len; front = x :: ft'; back} -> Append {len; front = ft'; back = x :: back}
    | Reverse _ | Append _ | End _ | Null -> rotation
  in
  let combine ({front = flen', _; rotation; _} as q') =
    let rotation' = comb @@ comb rotation in
    match rotation' with
    | End ft' -> {q' with front = (flen', ft'); rotation = Null}
    | Reverse _ | Append _ | Null -> {q' with rotation = rotation'}
  in
  if blen <= flen
  then combine q
  else
    let rev = Reverse {len = 0; front = ft; rev_front = []; back = bk; rev_back = []} in
    combine {front = (flen + blen, ft); rotation = rev; back = (0, [])}

let push x ({back = blen, bk; _} as q) = rebalance_ {q with back = (blen + 1, x :: bk)}

let pop {front = flen, ft; rotation; back} =
  let decrement rotation' =
    match rotation' with
    | Reverse ({len; _} as rv) -> Reverse {rv with len = len - 1}
    | Append {len = 0; back = _ :: bk'; _} -> End bk'
    | Append ({len; _} as rv) -> Append {rv with len = len - 1}
    | End _ | Null -> rotation'
  in
  match ft with
  | _ :: xs -> rebalance_ {front = (flen - 1, xs); rotation = decrement rotation; back}
  | [] -> raise Empty_queue
