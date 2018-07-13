(* STRUCTURE OF DEFERRED COMPUTATIONS WITH MEMOIZATION *)
type 'a mem = Delayed of (unit -> 'a) | Forced of 'a
type 'a t = 'a mem ref

let delay f = ref (Delayed f)

let force d =
  match !d with
  | Forced v -> v
  | Delayed f -> let v = f () in begin d := Forced v; v end
