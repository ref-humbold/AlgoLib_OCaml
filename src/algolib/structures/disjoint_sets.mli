(** Disjoint sets structure (union-find) *)

module type COMPARABLE =
sig
  type t
  val compare: t -> t -> int
end

module type DISJOINT_SETS =
sig
  type elem
  (** The type of elements of sets structure. *)

  type t
  (** The type of sets structures. *)

  val create: unit -> t
  (** [create ()] produces empty sets structure. *)

  val size: t -> int
  (** [size s] returns number of sets in structure [s]. *)

  val contains: elem -> t -> bool
  (** [contains x s] checks if [x] belongs to any set in structure [s]. *)

  val add_elem: elem -> t -> unit
  (** [add_elem x s] adds new singleton set with [x] to structure [s]. *)

  val find_set: elem -> t -> elem
  (** [find_set x s] finds the representant of [x] in structure [s]. *)

  val is_same_set: elem -> elem -> t -> bool
  (** [is_same_set x y s] checks if [x] and [y] belong to the same set in structure [s]. *)

  val union_set: elem -> elem -> t -> unit
  (** [union_set x y s] joins sets containing [x] and [y] in structure [s]. *)
end

module Make(Cmp: COMPARABLE): DISJOINT_SETS with type elem = Cmp.t
