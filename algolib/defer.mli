(** STRUCTURE OF DEFERRED COMPUTATIONS WITH MEMOIZATION *)
(** The type of deferred computations structure. *)
type 'a t

(** [delay f] creates new deferred computation. *)
val defer: (unit -> 'a) -> 'a t

(** [force d] forces deferred computation and memoizes the result. *)
val force: 'a t -> 'a
