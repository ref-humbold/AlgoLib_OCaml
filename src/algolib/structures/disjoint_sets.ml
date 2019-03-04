(* DISJOINT SETS STRUCTURE (UNION-FIND) *)
module type COMPARABLE =
sig
  type t
  val compare: t -> t -> int
end

module type DISJOINT_SETS =
sig
  type elem
  type t
  val create: unit -> t
  val size: t -> int
  val contains: elem -> t -> bool
  val add_elem: elem -> t -> unit
  val find_set: elem -> t -> elem
  val is_same_set: elem -> elem -> t -> bool
  val union_set: elem -> elem -> t -> unit
end

module Make(Cmp: COMPARABLE) =
struct
  type elem = Cmp.t

  module Repr = Map.Make(Cmp)

  type t = {mutable size: int; mutable map: elem Repr.t}

  let create (): t = {size=0; map=Repr.empty}

  let size {size; _} = size

  let contains element {map; _} = Repr.mem element map

  let add_elem element dset =
    if not @@ Repr.mem element dset.map
    then
      begin
        dset.size <- dset.size + 1;
        dset.map <- Repr.add element element dset.map
      end

  let rec find_set element dset =
    let value = Repr.find element dset.map in
    if Cmp.compare value element = 0
    then element
    else
      let repr = find_set value dset in
      begin
        dset.map <- Repr.add element repr dset.map;
        repr
      end

  let is_same_set element1 element2 dset =
    let repr1 = find_set element1 dset
    and repr2 = find_set element2 dset in
    Cmp.compare repr1 repr2 = 0

  let union_set element1 element2 dset =
    let repr1 = find_set element1 dset
    and repr2 = find_set element2 dset in
    if not @@ is_same_set repr1 repr2 dset
    then
      begin
        dset.size <- dset.size - 1;
        dset.map <- Repr.add repr1 repr2 dset.map
      end
end
