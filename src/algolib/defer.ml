(* Structure of defrerred computations with memoization. *)
type 'a mem = Deferred of (unit -> 'a) | Value of 'a

type 'a t = 'a mem ref

let defer f = ref (Deferred f)

let force d =
  match !d with
  | Value v -> v
  | Deferred f ->
    let v = f () in
    d := Value v ;
    v
