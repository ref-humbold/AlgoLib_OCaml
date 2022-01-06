(* Structure of real-time Hood-Melville queue *)
type 'a rot =
  | Rev of {len : int; frt : 'a list; rev_frt : 'a list; bck : 'a list; rev_bck : 'a list}
  | Apd of {len : int; frt : 'a list; bck : 'a list}
  | End of 'a list
  | Null

type 'a t = {front : int * 'a list; rotation : 'a rot; back : int * 'a list}

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
  let comb rt =
    match rt with
    | Rev {len; frt = x :: ft'; rev_frt; bck = y :: bk'; rev_bck} ->
      Rev {len; frt = ft'; rev_frt = x :: rev_frt; bck = bk'; rev_bck = y :: rev_bck}
    | Rev {len; frt = []; rev_frt; bck = [y]; rev_bck} ->
      Apd {len; frt = rev_frt; bck = y :: rev_bck}
    | Apd {len = 0; bck; _} -> End bck
    | Apd {len; frt = x :: ft'; bck} -> Apd {len; frt = ft'; bck = x :: bck}
    | Rev _ | Apd _ | End _ | Null -> rt
  in
  let combine ({front = flen', _; rotation = rt'; _} as q') =
    let rt'' = comb (comb rt') in
    match rt'' with
    | End ft'' -> {q' with front = (flen', ft''); rotation = Null}
    | Rev _ | Apd _ | Null -> {q' with rotation = rt''}
  in
  if blen <= flen
  then combine q
  else
    let rev = Rev {len = 0; frt = ft; rev_frt = []; bck = bk; rev_bck = []} in
    combine {front = (flen + blen, ft); rotation = rev; back = (0, [])}

let push x ({back = blen, bk; _} as q) = rebalance_ {q with back = (blen + 1, x :: bk)}

let pop {front = flen, ft; rotation = rt; back} =
  let decrement rt =
    match rt with
    | Rev ({len; _} as rv) -> Rev {rv with len = len - 1}
    | Apd {len = 0; bck = _ :: bk'; _} -> End bk'
    | Apd ({len; _} as rv) -> Apd {rv with len = len - 1}
    | End _ | Null -> rt
  in
  match ft with
  | _ :: xs -> rebalance_ {front = (flen - 1, xs); rotation = decrement rt; back}
  | [] -> raise Empty_queue
