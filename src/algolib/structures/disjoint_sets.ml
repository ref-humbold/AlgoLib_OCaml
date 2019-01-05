(* UNION-FIND DISJOINT SETS STRUCTURE *)
module type COMPARABLE =
sig
  type t
  type c = Less | Equal | Greater
  val cmp: t -> t -> c
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
  module Repr = Map.Make(
    struct
      type t = elem
      let compare x y =
        match Cmp.cmp x y with
        | Cmp.Less -> -1
        | Cmp.Equal -> 0
        | Cmp.Greater -> 1
    end)
  type t = {mutable size: int; mutable map: elem Repr.t}

  let create (): t = {size=0; map=Repr.empty}

  let size {size; _} = size

  let contains element {map; _} = Repr.mem element map

  let add_elem element dset =
    if Repr.mem element dset.map
    then ()
    else
      begin
        dset.size <- dset.size + 1;
        dset.map <- Repr.add element element dset.map
      end

  let rec find_set element dset =
    let value = Repr.find element dset.map in
    match Cmp.cmp value element with
    | Cmp.Equal -> element
    | Cmp.Less | Cmp.Greater ->
      let repr = find_set value dset in
      begin
        dset.map <- Repr.add element repr dset.map;
        repr
      end

  let is_same_set element1 element2 dset =
    let repr1 = find_set element1 dset
    and repr2 = find_set element2 dset in
    Cmp.cmp repr1 repr2 = Cmp.Equal

  let union_set element1 element2 dset =
    let repr1 = find_set element1 dset
    and repr2 = find_set element2 dset in
    if is_same_set repr1 repr2 dset
    then ()
    else
      begin
        dset.size <- dset.size - 1;
        dset.map <- Repr.add repr1 repr2 dset.map
      end
end
