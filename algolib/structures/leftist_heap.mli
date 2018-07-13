(** LEFTIST HEAP STRUCTURE *)
module type COMPARABLE =
sig
  type c = Less | Equal | Greater
  type t
  val cmp: t -> t -> c
end

module type HEAP =
sig
  (** The type of elements of heap. *)
  type elem

  (** The type of heaps. *)
  type t

  (** Exception raised when retrieving or removing elements from empty heap. *)
  exception EmptyHeap

  (** The empty heap. *)
  val empty: t

  (** [is_empty h] checks if heap [h] is empty. *)
  val is_empty: t -> bool

  (** [merge h1 h2] joins heaps [h1] and [h2] together. *)
  val merge: t -> t -> t

  (** [peek h] retrieves the least element from heap [h]. *)
  val peek: t -> elem

  (** [push x h] adds [x] to heap [h]. *)
  val push: elem -> t -> t

  (** [pop h] removes the least element from heap [h]. *)
  val pop: t -> t
end

module Make(Cmp: COMPARABLE): HEAP with type elem = Cmp.t
