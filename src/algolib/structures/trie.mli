(** Structure of trie *)

type t
(** The type of trie. *)

val empty : t
(** The empty trie. *)

val is_empty : t -> bool
(** [is_empty t] checks if trie [t] is empty. *)

val size : t -> int
(** [size t] returns number of texts in trie [t]. *)

val contains : string -> t -> bool
(** [contains x t] checks if text [s] is element of trie [t]. *)

val add : string -> t -> t
(** [add s t] adds text [s] to trie [t]. *)

val remove : string -> t -> t
(** [remove s t] removes text [s] from trie [t]. *)

val of_seq : string Seq.t -> t
(** [of_seq xs] creates new trie with elements of Seq [xs]. *)

val of_list : string list -> t
(** [of_list xs] creates new trie with elements of list [xs]. *)
