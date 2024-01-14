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
(** [is_empty list] checks if indexed list [list] is empty. *)

val length : 'a t -> int
(** [length list] returns number of elements in indexed list [list]. *)

val head : 'a t -> 'a
(** [head list] retrieves the first element of indexed list [list].

    @raise Empty_list if list is empty. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x list] adds element [x] at the front of indexed list [list]. *)

val ( @:: ) : 'a -> 'a t -> 'a t
(** [x @:: list] is [cons x list]. Right associative. *)

val tail : 'a t -> 'a t
(** [tail list] returns indexed list without first element of indexed list [list].

    @raise Empty_list if list is empty. *)

val get : int -> 'a t -> 'a
(** [get i list] gets element with index [i] in indexed list [list].

    @raise Invalid_index if index is negative or exceeds list length. *)

val ( &! ) : 'a t -> int -> 'a
(** [list &! i] is [get i list]. Left associative. *)

val set : int -> 'a -> 'a t -> 'a t
(** [set i x list] sets [x] as element with index [i] in indexed list [list].

    @raise Invalid_index if index is negative or exceeds list length. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq xs] creates new indexed list with elements of Seq [xs]. *)

val of_list : 'a list -> 'a t
(** [of_list xs] creates new indexed listwith elements of list [xs]. *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq list] converts indexed list [list] to Seq containing the same elements. *)

val to_list : 'a t -> 'a list
(** [to_list list] converts indexed list [list] to list containing the same elements. *)
