(* UNION-FIND DISJOINT SETS STRUCTURE *)
module type COMPARABLE =
sig
  type t
  type c = Less | Equal | Greater
  val cmp: t -> t -> c
end

module type DISJOINT_SETS =
sig
  (** The type of elements of sets structure. *)
  type elem

  (** The type of sets structures. *)
  type t

  (** [create ()] produces empty sets structure. *)
  val create: unit -> t

  (** [size s] returns number of sets in structure [s]. *)
  val size: t -> int

  (** [contains x s] checks if [x] belongs to any set in structure [s]. *)
  val contains: elem -> t -> bool

  (** [add_elem x s] adds new singleton set with [x] to structure [s]. *)
  val add_elem: elem -> t -> unit

  (** [find_set x s] finds the representant of [x] in structure [s]. *)
  val find_set: elem -> t -> elem

  (** [is_same_set x y s] checks if [x] and [y] belong to the same set in structure [s]. *)
  val is_same_set: elem -> elem -> t -> bool

  (** [union_set x y s] joins sets containing [x] and [y] in structure [s]. *)
  val union_set: elem -> elem -> t -> unit
end

module Make(Cmp: COMPARABLE): DISJOINT_SETS with type elem = Cmp.t
