(* Disjoint sets structure (union-find) *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type DISJOINT_SETS = sig
  type elem

  type t

  exception ElementPresent of elem

  val create : unit -> t

  val size : t -> int

  val contains : elem -> t -> bool

  val add_list : elem list -> t -> unit

  val add_seq : elem Seq.t -> t -> unit

  val find_set : elem -> t -> elem

  val find_set_opt : elem -> t -> elem option

  val is_same_set : elem -> elem -> t -> bool

  val union_set : elem -> elem -> t -> unit
end

module Make (Cmp : COMPARABLE) : DISJOINT_SETS with type elem = Cmp.t = struct
  type elem = Cmp.t

  module Repr = Map.Make (Cmp)

  type t = {mutable size : int; mutable map : elem Repr.t}

  exception ElementPresent of elem

  let create () : t = {size = 0; map = Repr.empty}

  let size {size; _} = size

  let contains element {map; _} = Repr.mem element map

  let add_list elements dset =
    List.iter (fun e -> if Repr.mem e dset.map then raise @@ ElementPresent e) elements ;
    List.iter
      (fun e ->
         dset.map <- Repr.add e e dset.map ;
         dset.size <- dset.size + 1)
      elements

  let add_seq elements dset = add_list (List.of_seq elements) dset

  let rec find_set element dset =
    let value = Repr.find element dset.map in
    if Cmp.compare value element = 0
    then element
    else
      let repr = find_set value dset in
      dset.map <- Repr.add element repr dset.map ;
      repr

  let find_set_opt element dset =
    try Some (find_set element dset) with
    | Not_found -> None

  let is_same_set element1 element2 dset =
    let repr1 = find_set element1 dset and repr2 = find_set element2 dset in
    Cmp.compare repr1 repr2 = 0

  let union_set element1 element2 dset =
    let repr1 = find_set element1 dset and repr2 = find_set element2 dset in
    if not @@ is_same_set repr1 repr2 dset
    then (
      dset.size <- dset.size - 1 ;
      dset.map <- Repr.add repr1 repr2 dset.map )
end
