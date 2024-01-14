(** Structure of pairing heap *)

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type HEAP = sig
  type elem
  (** The type of elements of heap. *)

  type t
  (** The type of heaps. *)

  exception Empty_heap
  (** Exception raised when retrieving or removing elements from empty heap. *)

  val empty : t
  (** The empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] checks if heap [h] is empty. *)

  val length : t -> int
  (** [length h] returns number of elements in heap [h]. *)

  val merge : t -> t -> t
  (** [merge h1 h2] joins heaps [h1] and [h2] together. *)

  val peek : t -> elem
  (** [peek h] retrieves the least element from heap [h].

      @raise Empty_heap if heap is empty. *)

  val peek_opt : t -> elem option
  (** [peek h] retrieves the least element from heap [h], or [None] if heap is empty. *)

  val push : elem -> t -> t
  (** [push x h] adds [x] to heap [h]. *)

  val pop : t -> t
  (** [pop h] removes the least element from heap [h].

      @raise Empty_heap if heap is empty. *)

  val of_seq : elem Seq.t -> t
  (** [of_list xs] creates new heap with elements of Seq [xs]. *)

  val of_list : elem list -> t
  (** [of_list xs] creates new heap with elements of list [xs]. *)
end

module Make (Cmp : COMPARABLE) : HEAP with type elem = Cmp.t
