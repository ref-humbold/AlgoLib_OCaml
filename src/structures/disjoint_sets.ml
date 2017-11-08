(* STRUKTURA ZBIORÓW ROZŁĄCZNYCH UNION-FIND *)
module Make(Cmp: COMPARABLE) : DISJOINT_SETS =
struct
  module Elem = Cmp
  module Repr = Map.Make(struct type t = Elem.t let compare x y = Elem.cmp x y end)
  type t = int * (Elem.t Repr.t)

  let create () = (0, Repr.empty)

  let size (n, _) = n

  let contains element (_, ds) = Repr.mem element ds

  let make_set element ((n, ds) as dset) =
    if Repr.mem element ds
    then dset
    else (n + 1, Repr.add element element ds)

  let rec find_set element ((_, ds) as dset) =
    let value = Repr.find element ds in
    if Elem.cmp value element != 0
    then let (repr, (new_n, new_ds)) = find_set value dset in
      (repr, (new_n, Repr.add element repr new_ds))
    else (element, dset)

  let union_set ((element1, element2) as e) dset =
    let (same, dset0) = is_same_set e dset in
    if same
    then dset0
    else let (repr1, new_ds1) = find_set element1 ds in
      let (repr2, (new_n, new_ds2)) = find_set element2 new_ds1 in
      (new_n - 1, Repr.add repr1 repr2 new_ds2)

  let is_same_set (element1, element2) dset =
    let (repr1, new_ds1) = find_set element1 dset in
    let (repr2, new_ds2) = find_set element2 new_ds1 in
    (Elem.cmp repr1 repr2 = 0, new_ds2)
end
