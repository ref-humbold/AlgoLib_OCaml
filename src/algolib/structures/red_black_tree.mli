module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type RBTREE = sig
  type elem
  (** The type of elements of tree. *)

  type t
  (** The type of trees. *)

  val empty : t
  (** The empty tree. *)

  val is_empty : t -> bool
  (** [is_empty t] checks if tree [t] is empty. *)

  val size : t -> int
  (** [size t] returns number of elements in tree [t]. *)

  val contains : elem -> t -> bool
  (** [contains x t] checks if [x] is element of tree [t]. *)

  val add : elem -> t -> t
  (** [add x t] adds [x] to tree [t]. *)

  val add_seq : elem Seq.t -> t -> t
  (** [add_seq xs t] adds elements from Seq [xs] to tree [t]. *)

  val add_list : elem list -> t -> t
  (** [add_list xs t] adds elements from list [xs] to tree [t]. *)

  val of_seq : elem Seq.t -> t
  (** [of_seq xs] creates new tree with elements of Seq [xs]. *)

  val of_list : elem list -> t
  (** [of_list xs] creates new tree with elements of list [xs]. *)

  val to_seq : t -> elem Seq.t
  (** [to_seq t] converts tree [t] to Seq containing the same elements in ascending order. *)

  val to_rev_seq : t -> elem Seq.t
  (** [to_rev_seq t] converts tree [t] to Seq containing the same elements in descending order. *)

  val to_list : t -> elem list
  (** [to_list t] converts tree [t] to list containing the same elements in ascending order. *)

  val to_rev_list : t -> elem list
  (** [to_rev_list t] converts tree [t] to list containing the same elements in descending order. *)
end

module Make (Cmp : COMPARABLE) : RBTREE with type elem = Cmp.t
