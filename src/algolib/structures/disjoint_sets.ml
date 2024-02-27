(* Structure of disjoint sets (union-find). *)
module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type DISJOINT_SETS = sig
  type elem

  type t

  exception Element_present of elem

  exception Duplicate_elements of elem list

  val create : unit -> t

  val size : t -> int

  val contains : elem -> t -> bool

  val add_seq : ?represent:elem -> elem Seq.t -> t -> unit

  val add_list : ?represent:elem -> elem list -> t -> unit

  val find_set : elem -> t -> elem

  val find_set_opt : elem -> t -> elem option

  val is_same_set : elem -> elem -> t -> bool

  val union_set : elem -> elem -> t -> unit

  val of_seq : elem Seq.t Seq.t -> t

  val of_list : elem list list -> t
end

module Make (Cmp : COMPARABLE) : DISJOINT_SETS with type elem = Cmp.t = struct
  type elem = Cmp.t

  type t = {mutable size : int; map : (elem, elem) Hashtbl.t}

  exception Element_present of elem

  exception Duplicate_elements of elem list

  let create () : t = {size = 0; map = Hashtbl.create 37}

  let size {size; _} = size

  let contains element {map; _} = Hashtbl.mem map element

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

  let add_seq ?represent elements dset =
    let represent_ =
      match represent with
      | Some r -> Some (find_set r dset)
      | None ->
        ( match Seq.uncons elements with
          | None -> None
          | Some (e, _) -> Some e )
    in
    match represent_ with
    | Some r ->
      Seq.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ Element_present e) elements ;
      Seq.iter (fun e -> Hashtbl.add dset.map e r) elements ;
      dset.size <- dset.size + 1
    | None -> ()

  let add_list ?represent elements dset =
    let represent_ =
      match represent with
      | Some r -> Some (find_set r dset)
      | None ->
        ( match elements with
          | [] -> None
          | e :: _ -> Some e )
    in
    match represent_ with
    | Some r ->
      List.iter (fun e -> if Hashtbl.mem dset.map e then raise @@ Element_present e) elements ;
      List.iter (fun e -> Hashtbl.add dset.map e r) elements ;
      dset.size <- dset.size + 1
    | None -> ()

  let is_same_set element1 element2 dset =
    let repr1 = find_set element1 dset and repr2 = find_set element2 dset in
    Cmp.compare repr1 repr2 = 0

  let union_set element1 element2 dset =
    let repr1 = find_set element1 dset and repr2 = find_set element2 dset in
    if not @@ is_same_set repr1 repr2 dset
    then (
      dset.size <- dset.size - 1 ;
      Hashtbl.add dset.map repr1 repr2 )

  module ElemSet = Set.Make (Cmp)

  let validate_duplicates_ sets =
    let module CountsMap = Map.Make (Cmp) in
    let counts =
      List.fold_left
        (fun cnt set ->
           ElemSet.fold
             (fun k cnt' ->
                CountsMap.update
                  k
                  (fun v ->
                     match v with
                     | None -> Some 1
                     | Some v' -> Some (v' + 1) )
                  cnt' )
             set
             cnt )
        CountsMap.empty
        sets
    in
    let duplicates = CountsMap.fold (fun k v acc -> if v > 1 then k :: acc else acc) counts [] in
    match duplicates with
    | [] -> ()
    | _ -> raise @@ Duplicate_elements duplicates

  let of_sets_ sets =
    validate_duplicates_ sets ;
    let repr_seq =
      Seq.concat_map (fun s ->
                       let repr = ElemSet.min_elt s in
                       Seq.map (fun e -> (e, repr)) @@ ElemSet.to_seq s )
      @@ List.to_seq sets
    in
    {size = List.length sets; map = Hashtbl.of_seq repr_seq}

  let of_seq xs = of_sets_ @@ List.of_seq @@ Seq.map ElemSet.of_seq xs

  let of_list xs = of_sets_ @@ List.map ElemSet.of_list xs
end
