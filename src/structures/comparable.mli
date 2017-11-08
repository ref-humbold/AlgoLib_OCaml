module type COMPARABLE =
sig
  type t
  val cmp: t -> t -> int
end
