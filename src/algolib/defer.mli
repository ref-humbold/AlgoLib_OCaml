(** Structure of defrerred computations with memoization *)

type 'a t
(** The type of deferred computations *)

val defer: (unit -> 'a) -> 'a t
(** [delay f] creates new deferred computation *)

val force: 'a t -> 'a
(** [force d] forces deferred computation and memoizes the result *)
