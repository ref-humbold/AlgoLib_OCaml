(** INDEXED LIST STRUCTURE *)
type 'a t
val empty: 'a t
val is_empty: 'a t -> bool
val head: 'a t -> 'a
val cons: 'a -> 'a t -> 'a t
val tail: 'a t -> 'a t
val elem: int -> 'a t -> 'a
val update: int -> 'a -> 'a t -> 'a t
