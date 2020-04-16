(** Structure of disjoint sets (union-find) *)

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type DISJOINT_SETS = sig
  (** The type of elements of sets structure. *)
  type elem

  (** The type of sets structures. *)
  type t

  (** Exception raised when adding already present element. *)
  exception ElementPresent of elem

  val create : unit -> t
  (** [create ()] produces empty sets structure. *)

  val size : t -> int
  (** [size s] returns number of sets in structure [s]. *)

  val contains : elem -> t -> bool
  (** [contains x s] checks if [x] belongs to any set in structure [s]. *)

  val add_seq : elem Seq.t -> t -> unit
  (** [add_seq xs s] adds new singleton sets from Seq [xs] to structure [s]. *)

  val add_list : elem list -> t -> unit
  (** [add_list xs s] adds new singleton sets from list [xs] to structure [s]. *)

  val find_set : elem -> t -> elem
  (** [find_set x s] finds the representant of [x] in structure [s]. Raises [Not_found] when [x] is
      not present in the structure. *)

  val find_set_opt : elem -> t -> elem option
  (** [find_set_opt x d s] finds the representant of [x] in structure [s] if it exists, otherwise
      returns [None]. *)

  val is_same_set : elem -> elem -> t -> bool
  (** [is_same_set x y s] checks if [x] and [y] belong to the same set in structure [s]. Raises
      [Not_found] when neither [x] nor [y] is present in the structure. *)

  val union_set : elem -> elem -> t -> unit
  (** [union_set x y s] joins sets containing [x] and [y] in structure [s]. Raises [Not_found] when
      neither [x] nor [y] is present in the structure. *)

  val of_seq : elem Seq.t -> t
  (** [of_seq xs s] creates new structure [s] of singleton sets from elements of Seq [xs]. *)

  val of_list : elem list -> t
  (** [of_list xs s] creates new structure [s] of singleton sets from elements of list [xs]. *)
end

module Make (Cmp : COMPARABLE) : DISJOINT_SETS with type elem = Cmp.t
