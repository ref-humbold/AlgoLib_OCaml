(* STRUCTURE OF DEFERRED COMPUTATIONS WITH MEMOIZATION *)
type 'a mem = Deferred of (unit -> 'a) | Forced of 'a
type 'a t = 'a mem ref

let defer f = ref (Deferred f)

let force d =
  match !d with
  | Forced v -> v
  | Deferred f -> let v = f () in begin d := Forced v; v end
