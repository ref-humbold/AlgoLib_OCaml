(** UNION-FIND DISJOINT SETS STRUCTURE *)
module type COMPARABLE =
sig
  type c = Less | Equal | Greater
  type t
  val cmp: t -> t -> c
end

module type DISJOINT_SETS =
sig
  type elem
  type t
  val empty: t
  val size: t -> int
  val contains: elem -> t -> bool
  val add_elem: elem -> t -> t
  val find_set: elem -> t -> elem * t
  val is_same_set: elem * elem -> t -> bool * t
  val union_set: elem * elem -> t -> t
end

module Make(Cmp: COMPARABLE) =
struct
  type elem = Cmp.t
  module Repr = Map.Make(struct
      type t = elem
      let compare x y =
        match Cmp.cmp x y with
        | Less -> -1
        | Equal -> 0
        | Greater -> 1
    end)
  type t = int * (elem Repr.t)

  let empty = (0, Repr.empty)

  let size (n, _) = n

  let contains element (_, ds) = Repr.mem element ds

  let add_elem element ((n, ds) as dset) =
    if Repr.mem element ds
    then dset
    else (n + 1, Repr.add element element ds)

  let rec find_set element ((_, ds) as dset) =
    let value = Repr.find element ds in
    match Cmp.cmp value element with
    | Equal -> (element, dset)
    | Less | Greater ->
      let (repr, (new_n, new_ds)) = find_set value dset in
      (repr, (new_n, Repr.add element repr new_ds))

  let is_same_set (element1, element2) dset =
    let (repr1, new_ds1) = find_set element1 dset in
    let (repr2, new_ds2) = find_set element2 new_ds1 in
    (Cmp.cmp repr1 repr2 = Equal, new_ds2)

  let union_set ((element1, element2) as e) dset =
    let (same, dset0) = is_same_set e dset in
    if same
    then dset0
    else let (repr1, new_ds1) = find_set element1 dset0 in
      let (repr2, (new_n, new_ds2)) = find_set element2 new_ds1 in
      (new_n - 1, Repr.add repr1 repr2 new_ds2)
end
