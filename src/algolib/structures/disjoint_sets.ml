(* Structure of disjoint sets (union-find). *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type DISJOINT_SETS = sig
  type elem

  type t

  exception Element_present of elem

  val create : unit -> t

  val size : t -> int

  val contains : elem -> t -> bool

  val add : elem -> t -> unit

  val add_seq : elem Seq.t -> t -> unit

  val add_list : elem list -> t -> unit

  val find_set : elem -> t -> elem

  val find_set_opt : elem -> t -> elem option

  val is_same_set : elem -> elem -> t -> bool

  val union_set : elem -> elem -> t -> unit

  val of_seq : elem Seq.t -> t

  val of_list : elem list -> t
end

module Make (Cmp : COMPARABLE) : DISJOINT_SETS with type elem = Cmp.t = struct
  type elem = Cmp.t

  type t = {mutable size : int; map : (elem, elem) Hashtbl.t}

  exception Element_present of elem

  let create () : t = {size = 0; map = Hashtbl.create 37}

  let size {size; _} = size

  let contains element {map; _} = Hashtbl.mem map element

  let add_ element dset =
    Hashtbl.add dset.map element element ;
    dset.size <- dset.size + 1

  let add element dset =
    if Hashtbl.mem dset.map element then raise @@ Element_present element else add_ element dset

  let add_seq elements dset =
    Seq.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ Element_present e) elements ;
    Seq.iter (fun e -> add_ e dset) elements

  let add_list elements dset =
    List.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ Element_present e) elements ;
    List.iter (fun e -> add_ e dset) elements

  let rec find_set element dset =
    let value = Hashtbl.find dset.map element in
    if Cmp.compare value element = 0
    then element
    else
      let repr = find_set value dset in
      Hashtbl.add dset.map element repr ; repr

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
      Hashtbl.add dset.map repr1 repr2 )

  let of_seq xs =
    { size = Seq.fold_left (fun acc _ -> acc + 1) 0 xs;
      map = Hashtbl.of_seq @@ Seq.map (fun x -> (x, x)) xs }

  let of_list xs =
    {size = List.length xs; map = Hashtbl.of_seq @@ Seq.map (fun x -> (x, x)) @@ List.to_seq xs}
end
