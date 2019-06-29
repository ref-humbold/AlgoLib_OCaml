(** Red-black tree structure. *)

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

  val to_list : t -> elem list
  (** [to_list t] converts tree [t] to list containing the same elements in ascending order. *)

  val contains : elem -> t -> bool
  (** [contains x t] checks if [x] is element of tree [t]. *)

  val add : elem -> t -> t
  (** [add x t] adds [x] to tree [t]. *)
end

module Make (Cmp : COMPARABLE) : RBTREE with type elem = Cmp.t
