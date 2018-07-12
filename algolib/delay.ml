(* STRUCTURE OF DEFERRED COMPUTATIONS WITH MEMOIZATION *)
type 'a t = 'a mem ref
type 'a mem = Delayed of (unit -> 'a) | Forced of 'a

let delay f = ref (Delayed f)

let force d =
  match !d with
  | Forced v -> v
  | Delayed f -> let v = f () in begin d := Forced v; v end
