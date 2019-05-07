(** Indexed list structure. *)

(** The type of indexed lists. *)
type 'a t

(** Exception raised when retrieving or removing elements from empty indexed lists. *)
exception EmptyList

(** Exception raised when index is out of range. *)
exception InvalidIndex

val empty : 'a t
(** The empty indexed list. *)

val is_empty : 'a t -> bool
(** [is_empty lst] checks if indexed list [lst] is empty. *)

val head : 'a t -> 'a
(** [head lst] retrieves the first element of indexed list [lst].
    @raise EmptyList if list is empty. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x lst] adds element [x] at the front of indexed list [lst]. *)

val tail : 'a t -> 'a t
(** [tail lst] returns indexed list without first element of indexed list [lst].
    @raise EmptyList if list is empty. *)

val elem : int -> 'a t -> 'a
(** [elem i lst] retrieves element indexed with [i] in indexed list [lst].
    @raise InvalidIndex if index is negative or exceeds list length. *)

val update : int -> 'a -> 'a t -> 'a t
(** [update i x lst] sets [x] as element indexed with [i] in indexed list [lst].
    @raise InvalidIndex if index is negative or exceeds list length. *)
