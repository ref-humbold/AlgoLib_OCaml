(** Structure of defrerred computations with memoization. *)

(** The type of deferred computations. *)
type 'a t

val defer : (unit -> 'a) -> 'a t
(** [defer f] creates new deferred computation. *)

val ( ~$ ) : (unit -> 'a) -> 'a t
(** [( ~$ )] is a prefix alias of [defer]. *)

val force : 'a t -> 'a
(** [force d] forces deferred computation and memoizes the result. *)

val ( !$ ) : 'a t -> 'a
(** [( !$ )] is a prefix alias of [force]. *)
