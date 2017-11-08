module type RED_BLACK_TREE =
sig
  module Elem: COMPARABLE
  type t
  val create: unit -> set
  val is_empty: set -> bool
  val size: set -> int
  val to_list: set -> 'a list
  val contains: elem -> set -> bool
  val add: elem -> set -> set
end

module Make: functor (Cmp: COMPARABLE) -> RED_BLACK_TREE
