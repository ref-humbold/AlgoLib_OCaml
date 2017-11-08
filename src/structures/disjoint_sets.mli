module type DISJOINT_SETS =
sig
  module Elem: COMPARABLE
  type t
  val create: unit -> t
  val size: t -> int
  val contains: elem -> t -> bool
  val make_set: elem -> t -> t
  val find_set: elem -> t -> t
  val union_set: elem * elem -> t -> t
  val is_same_set: elem * elem -> t -> bool * t
end

module Make: functor (Cmp: COMPARABLE) -> DISJOINT_SETS
