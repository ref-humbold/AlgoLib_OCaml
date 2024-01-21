(* Structure of double-ended queue. *)
type 'a t = 'a list * 'a list

exception Empty_deque

let empty = ([], [])

let is_empty deque =
  match deque with
  | [], [] -> true
  | _ -> false

let length deque = (List.length @@ fst deque) + (List.length @@ snd deque)

let front deque =
  match deque with
  | e :: _, _ | [], e :: _ -> e
  | [], [] -> raise Empty_deque

let back deque =
  match deque with
  | _, e :: _ | e :: _, [] -> e
  | [], [] -> raise Empty_deque

let push_front element deque =
  match deque with
  | [x], [] -> ([element], [x])
  | [x1; x2], [y] -> ([element; x1], [y; x2])
  | ft, bk -> (element :: ft, bk)

let push_back element deque =
  match deque with
  | [], [x] -> ([x], [element])
  | [y], [x1; x2] -> ([y; x2], [element; x1])
  | ft, bk -> (ft, element :: bk)

let balance_ lst =
  let rec balance_' lst' ft n =
    match lst' with
    | x :: xs -> if n > 0 then balance_' xs (x :: ft) (n - 1) else (List.rev ft, List.rev lst')
    | [] -> ([], [])
  in
  balance_' lst [] (List.length lst / 2)

let pop_front deque =
  match deque with
  | [_], bk -> balance_ bk
  | _ :: ft, bk -> (ft, bk)
  | [], _ :: bk -> ([], bk)
  | [], [] -> raise Empty_deque

let pop_back deque =
  match deque with
  | ft, [_] -> balance_ ft
  | ft, _ :: bk -> (ft, bk)
  | _ :: ft, [] -> (ft, [])
  | [], [] -> raise Empty_deque

let of_seq xs = Seq.fold_left (fun acc e -> push_back e acc) empty xs

let of_list xs =
  match xs with
  | [] -> empty
  | [x] -> ([], [x])
  | [x1; x2] -> ([x1], [x2])
  | _ ->
    let rec split n lst =
      if n <= 0
      then ([], List.rev lst)
      else
        match lst with
        | [] -> ([], [])
        | y :: ys ->
          let ft, bk = split (n - 1) ys in
          (y :: ft, bk)
    in
    split (List.length xs / 2) xs
