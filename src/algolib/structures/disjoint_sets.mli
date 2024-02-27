module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type DISJOINT_SETS = sig
  type elem
  (** The type of elements of sets structure. *)

  type t
  (** The type of sets structures. *)

  exception Element_present of elem
  (** Exception raised when adding already present element. *)

  exception Duplicate_elements of elem list
  (** Exception raised when creating the structure with duplicated elements. *)

  val create : unit -> t
  (** [create ()] produces empty sets structure. *)

  val size : t -> int
  (** [size s] returns number of sets in structure [s]. *)

  val contains : elem -> t -> bool
  (** [contains x s] checks if [x] belongs to any set in structure [s]. *)

  val add_seq : ?represent:elem -> elem Seq.t -> t -> unit
  (** [add_seq e xs s] adds elements from Seq [xs] as as a new set or to the existing set
      represented by element [e] in structure [s].

      @raise Element_present if any value from [xs] is already present in the structure. *)

  val add_list : ?represent:elem -> elem list -> t -> unit
  (** [add_list e xs s] adds elements from list [xs] as a new set or to the existing set represented
      by element [e] in structure [s].

      @raise Element_present if any value from [xs] is already present in the structure. *)

  val find_set : elem -> t -> elem
  (** [find_set x s] finds the represent of [x] in structure [s].

      @raise Not_found if [x] is not present in the structure. *)

  val find_set_opt : elem -> t -> elem option
  (** [find_set_opt x s] finds the represent of [x] in structure [s] if it exists, otherwise returns
      [None]. *)

  val is_same_set : elem -> elem -> t -> bool
  (** [is_same_set x y s] checks if [x] and [y] belong to the same set in structure [s].

      @raise Not_found if neither [x] nor [y] is present in the structure. *)

  val union_set : elem -> elem -> t -> unit
  (** [union_set x y s] joins sets containing [x] and [y] in structure [s].

      @raise Not_found if neither [x] nor [y] is present in the structure. *)

  val of_seq : elem Seq.t Seq.t -> t
  (** [of_seq xs] creates new structure of sets from elements of Seq [xs]. *)

  val of_list : elem list list -> t
  (** [of_list xs] creates new structure of sets from elements of list [xs]. *)
end

module Make (Cmp : COMPARABLE) : DISJOINT_SETS with type elem = Cmp.t
