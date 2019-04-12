(* RED-BLACK TREE STRUCTURE *)
module type COMPARABLE =
sig
  type t
  val compare: t -> t -> int
end

module type RBTREE =
sig
  (** The type of elements of tree. *)
  type elem

  (** The type of trees. *)
  type t

  (** The empty tree. *)
  val empty: t

  (** [is_empty t] checks if tree [t] is empty. *)
  val is_empty: t -> bool

  (** [size t] returns number of elements in tree [t]. *)
  val size: t -> int

  (** [to_list t] converts tree [t] to list containing the same elements in ascending order. *)
  val to_list: t -> elem list

  (** [contains x t] checks if [x] is element of tree [t]. *)
  val contains: elem -> t -> bool

  (** [add x t] adds [x] to tree [t]. *)
  val add: elem -> t -> t
end

module Make(Cmp: COMPARABLE): RBTREE with type elem := Cmp.t