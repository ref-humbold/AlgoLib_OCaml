(** Indexed list structure *)

type 'a t
(** The type of indexed lists *)

exception EmptyList
(** Exception raised when retrieving or removing elements from empty indexed lists *)

exception InvalidIndex
(** Exception raised when index is out of range *)

val create: unit -> 'a t
(** [create ()] returns a new indexed list *)

val is_empty: 'a t -> bool
(** [is_empty lst] checks if indexed list [lst] is empty *)

val head: 'a t -> 'a
(** [head lst] retrieves the first element of indexed list [lst] *)

val cons: 'a -> 'a t -> 'a t
(** [cons x lst] adds element [x] at the front of indexed list [lst] *)

val tail: 'a t -> 'a t
(** [tail lst] returns indexed list without first element of indexed list [lst] *)

val elem: int -> 'a t -> 'a
(** [elem n lst] retrieves element indexed with [n] in indexed list [lst] *)

val update: int -> 'a -> 'a t -> 'a t
(** [update n x lst] sets [x] as element indexed with [n] in indexed list [lst] *)
