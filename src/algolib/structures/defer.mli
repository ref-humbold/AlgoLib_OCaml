(** Structure of defrerred computations with memoization *)

type 'a t
(** The type of deferred computations. *)

val defer : (unit -> 'a) -> 'a t
(** [defer f] creates new deferred computation [f]. *)

val ( ~$ ) : (unit -> 'a) -> 'a t
(** [~$ f] is [defer f]. *)

val force : 'a t -> 'a
(** [force d] forces deferred computation [d] and memoizes the result. *)

val ( !$ ) : 'a t -> 'a
(** [!$ d] is [force d]. *)
