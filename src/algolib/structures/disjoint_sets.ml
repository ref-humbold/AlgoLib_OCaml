(* Structure of disjoint sets (union-find) *)
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

  exception ElementPresent of elem

  let create () : t = {size = 0; map = Hashtbl.create 37}

  let size {size; _} = size

  let contains element {map; _} = Hashtbl.mem map element

  let add_seq elements dset =
    Seq.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ ElementPresent e) elements ;
    Seq.iter
      (fun e ->
         Hashtbl.add dset.map e e ;
         dset.size <- dset.size + 1)
      elements

  let add_list elements dset =
    List.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ ElementPresent e) elements ;
    List.iter
      (fun e ->
         Hashtbl.add dset.map e e ;
         dset.size <- dset.size + 1)
      elements

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

  let of_seq elements =
    { size = Seq.fold_left (fun acc _ -> acc + 1) 0 elements;
      map = Hashtbl.of_seq @@ Seq.map (fun x -> (x, x)) elements }

  let of_list elements =
    { size = List.length elements;
      map = Hashtbl.of_seq @@ Seq.map (fun x -> (x, x)) @@ List.to_seq elements }
end
