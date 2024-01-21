(* Structure of defrerred computations with memoization. *)
type 'a mem = Deferred of (unit -> 'a) | Value of 'a

type 'a t = 'a mem ref

let defer func = ref (Deferred func)

let ( ~$ ) = defer

let force df =
  match !df with
  | Value v -> v
  | Deferred f ->
    let v = f () in
    df := Value v ;
    v

let ( !$ ) = force
