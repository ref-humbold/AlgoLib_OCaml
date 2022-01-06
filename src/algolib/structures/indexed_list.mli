(** Structure of indexed list *)

type 'a t
(** The type of indexed lists. *)

exception Empty_list
(** Exception raised when retrieving or removing elements from empty indexed lists. *)

exception Invalid_index
(** Exception raised when index is out of range. *)

val empty : 'a t
(** The empty indexed list. *)

val is_empty : 'a t -> bool
(** [is_empty lst] checks if indexed list [lst] is empty. *)

val to_list : 'a t -> 'a list
(** [to_list lst] converts indexed list [lst] to list containing the same elements. *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq lst] converts indexed list [lst] to sequence containing the same elements. *)

val head : 'a t -> 'a
(** [head lst] retrieves the first element of indexed list [lst].

    @raise Empty_list if list is empty. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x lst] adds element [x] at the front of indexed list [lst]. *)

val ( @:: ) : 'a -> 'a t -> 'a t
(** [( @:: )] is an infix alias of [cons]. Right associative. *)

val tail : 'a t -> 'a t
(** [tail lst] returns indexed list without first element of indexed list [lst].

    @raise Empty_list if list is empty. *)

val elem : int -> 'a t -> 'a
(** [elem i lst] retrieves element indexed with [i] in indexed list [lst].

    @raise Invalid_index if index is negative or exceeds list length. *)

val ( *! ) : 'a t -> int -> 'a
(** [( *! )] is an infix alias of [elem] with its arguments swapped. Left associative. *)

val update : int -> 'a -> 'a t -> 'a t
(** [update i x lst] sets [x] as element indexed with [i] in indexed list [lst].

    @raise Invalid_index if index is negative or exceeds list length. *)
