(** INDEXED LIST STRUCTURE *)
(** The type of indexed lists. *)
type 'a t

(** Exception raised when retrieving or removing elements from empty indexed lists. *)
exception EmptyList

(** Exception raised when index is out of range. *)
exception InvalidIndex

(** [create ()] returns a new indexed list. *)
val create: unit -> 'a t

(** [is_empty lst] checks if indexed list [lst] is empty. *)
val is_empty: 'a t -> bool

(** [size lst] returns number of elements in indexed list [lst]. *)
val size: 'a t -> int

(** [head lst] retrieves the first element of indexed list [lst]. *)
val head: 'a t -> 'a

(** [cons x lst] adds element [x] at the front of indexed list [lst]. *)
val cons: 'a -> 'a t -> 'a t

(** [tail lst] returns indexed list without first element of indexed list [lst]. *)
val tail: 'a t -> 'a t

(** [elem n lst] retrieves element indexed with [n] in indexed list [lst]. *)
val elem: int -> 'a t -> 'a

(** [update n x lst] sets [x] as element indexed with [n] in indexed list [lst]. *)
val update: int -> 'a -> 'a t -> 'a t
