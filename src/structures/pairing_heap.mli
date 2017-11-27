module type PAIRING_HEAP =
sig
  module Elem: COMPARABLE
  type t
  val create: unit -> t
  val is_empty: t -> bool
  val get: t -> 'a
  val push: 'a -> t -> t
  val pop: t -> t
  val merge: t -> t -> t
end

module Make: functor (Cmp: COMPARABLE) -> PAIRING_HEAP
