(* Tests: Disjoint sets structure (union-find). *)
open OUnit2
open OAssert
open Algolib.Structures.Disjoint_sets

let numbers = [10; 6; 14; 97; 24; 37; 2; 30; 45; 18; 51; 71; 68; 26]

let present = List.filteri (fun i _ -> i mod 3 = 2) numbers

let absent = [111; 140; 187; 253]

module IntSets = Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

let of_numbers () = IntSets.of_list @@ List.map (fun n -> [n]) numbers

module IsOption = Is.Option.Of (Type.Int)

(* size_Test_list *)

let size__when_empty__then_zero =
  "size__when_empty__then_zero" >:: fun _ ->
    (* when *)
    let result = IntSets.size @@ IntSets.create () in
    (* then *)
    assert_that result @@ Is.Int.zero

let size__when_not_empty__then_sets_count =
  "size__when_not_empty__then_sets_count" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let result = IntSets.size test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to (List.length numbers)

let size_Test_list = test_list [size__when_empty__then_zero; size__when_not_empty__then_sets_count]

(* contains_Test_list *)

let contains__when_empty__then_false =
  "contains__when_empty__then_false" >:: fun _ ->
    (* given *)
    let test_object = IntSets.create () in
    (* when *)
    let result = IntSets.contains (List.hd numbers) test_object in
    (* then *)
    assert_that result Is.false_

let contains__when_present__then_true =
  "contains__when_present__then_true" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let result = IntSets.contains (List.hd present) test_object in
    (* then *)
    assert_that result Is.true_

let contains__when_absent__then_false =
  "contains__when_absent__then_false" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let result = IntSets.contains (List.hd absent) test_object in
    (* then *)
    assert_that result Is.false_

let contains_Test_list =
  test_list
    [ contains__when_empty__then_false;
      contains__when_present__then_true;
      contains__when_absent__then_false ]

(* add_seq_Test_list *)

let add_seq__when_empty__then_new_set =
  "add_seq__when_empty__then_new_set" >:: fun _ ->
    (* given *)
    let test_object = IntSets.create () in
    (* when *)
    IntSets.add_seq (List.to_seq numbers) test_object ;
    (* then *)
    List.iter
      (fun e ->
         assert_that (IntSets.contains e test_object) Is.true_ ;
         assert_that (IntSets.find_set e test_object) @@ Is.Int.equal_to (List.hd numbers) )
      numbers ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to 1

let add_seq__when_empty_new_elements__then_no_changes =
  "add_seq__when_empty_new_elements__then_no_changes" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    IntSets.add_seq Seq.empty test_object ;
    (* then *)
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers)

let add_seq__when_new_elements__then_new_set =
  "add_seq__when_new_elements__then_new_set" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    IntSets.add_seq (List.to_seq absent) test_object ;
    (* then *)
    let represent = List.fold_left min max_int absent in
    List.iter
      (fun e ->
         assert_that (IntSets.contains e test_object) Is.true_ ;
         assert_that (IntSets.find_set e test_object) @@ Is.Int.equal_to represent )
      absent ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers + 1)

let add_seq__when_present_elements__then_element_present =
  "add_seq__when_present_elements__then_element_present" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let exec () = IntSets.add_seq (List.to_seq present) test_object in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Element_present (List.hd present))

let add_seq__when_new_and_present_elements__then_element_present =
  "add_seq__when_present_element__then_element_present" >:: fun _ ->
    (* given *)
    let test_object = of_numbers ()
    and elements = Seq.append (List.to_seq absent) (List.to_seq present) in
    (* when *)
    let exec () = IntSets.add_seq elements test_object in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Element_present (List.hd present))

let add_seq_Test_list =
  test_list
    [ add_seq__when_empty__then_new_set;
      add_seq__when_empty_new_elements__then_no_changes;
      add_seq__when_new_elements__then_new_set;
      add_seq__when_present_elements__then_element_present;
      add_seq__when_new_and_present_elements__then_element_present ]

(* add_list_Test_list *)

let add_list__when_empty__then_new_set =
  "add_list__when_empty__then_new_set" >:: fun _ ->
    (* given *)
    let test_object = IntSets.create () in
    (* when *)
    IntSets.add_list numbers test_object ;
    (* then *)
    List.iter
      (fun e ->
         assert_that (IntSets.contains e test_object) Is.true_ ;
         assert_that (IntSets.find_set e test_object) @@ Is.Int.equal_to (List.hd numbers) )
      numbers ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to 1

let add_list__when_empty_new_elements__then_no_changes =
  "add_list__when_empty_new_elements__then_no_changes" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    IntSets.add_list [] test_object ;
    (* then *)
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers)

let add_list__when_new_elements__then_new_set =
  "add_list__when_new_elements__then_new_set" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    IntSets.add_list absent test_object ;
    (* then *)
    let represent = List.fold_left min max_int absent in
    List.iter
      (fun e ->
         assert_that (IntSets.contains e test_object) Is.true_ ;
         assert_that (IntSets.find_set e test_object) @@ Is.Int.equal_to represent )
      absent ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers + 1)

let add_list__when_present_elements__then_element_present =
  "add_list__when_present_elements__then_element_present" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let exec () = IntSets.add_list present test_object in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Element_present (List.hd present))

let add_list__when_new_and_present_elements__then_element_present =
  "add_list__when_present_element__then_element_present" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () and elements = absent @ present in
    (* when *)
    let exec () = IntSets.add_list elements test_object in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Element_present (List.hd present))

let add_list_Test_list =
  test_list
    [ add_list__when_empty__then_new_set;
      add_list__when_empty_new_elements__then_no_changes;
      add_list__when_new_elements__then_new_set;
      add_list__when_present_elements__then_element_present;
      add_list__when_new_and_present_elements__then_element_present ]

(* find_set_Test_list *)

let find_set__when_empty__then_not_found =
  "find_set__when_empty__then_not_found" >:: fun _ ->
    (* given *)
    let test_object = IntSets.create () in
    (* when *)
    let exec () = IntSets.find_set (List.hd numbers) test_object in
    (* then *)
    assert_that exec @@ Is.raising Not_found

let find_set__when_present__then_represent =
  "find_set__when_present__then_represent" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () and element = List.hd present in
    (* when *)
    let result = IntSets.find_set element test_object in
    (* then *)
    assert_that result @@ Is.Int.equal_to element

let find_set__when_absent__then_not_found =
  "find_set__when_absent__then_not_found" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let exec () = IntSets.find_set (List.hd absent) test_object in
    (* then *)
    assert_that exec @@ Is.raising Not_found

let find_set__when_same_set__then_same_represent =
  "find_set__when_same_set__then_same_represent" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list [numbers] in
    (* when *)
    let result1 = IntSets.find_set (List.nth numbers 0) test_object
    and result2 = IntSets.find_set (List.nth numbers 1) test_object in
    (* then *)
    assert_that result2 @@ Is.Int.equal_to result1

let find_set_Test_list =
  test_list
    [ find_set__when_empty__then_not_found;
      find_set__when_present__then_represent;
      find_set__when_absent__then_not_found;
      find_set__when_same_set__then_same_represent ]

(* find_set_opt_Test_list *)

let find_set_opt__when_present__then_some_with_represent =
  "find_set_opt__when_present__then_some_with_represent" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () and element = List.hd present in
    (* when *)
    let result = IntSets.find_set_opt element test_object in
    (* then *)
    assert_that result @@ IsOption.some element

let find_set_opt__when_absent__then_none =
  "find_set_opt__when_absent__then_none" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let result = IntSets.find_set_opt (List.hd absent) test_object in
    (* then *)
    assert_that result IsOption.none

let find_set_opt_Test_list =
  test_list
    [find_set_opt__when_present__then_some_with_represent; find_set_opt__when_absent__then_none]

(* union_set_Test_list *)

let union_set__when_different_sets__then_same_represent =
  "union_set__when_different_sets__then_same_represent" >:: fun _ ->
    (* given *)
    let test_object = of_numbers ()
    and element1 = List.nth present 0
    and element2 = List.nth present 1 in
    (* when *)
    IntSets.union_set element1 element2 test_object ;
    (* then *)
    assert_that (IntSets.is_same_set element1 element2 test_object) Is.true_ ;
    assert_that (IntSets.find_set element2 test_object)
    @@ Is.Int.equal_to (IntSets.find_set element1 test_object) ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers - 1)

let union_set__when_single_element__then_no_changes =
  "union_set__when_single_element__then_no_changes" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () and element = List.hd present in
    (* when *)
    IntSets.union_set element element test_object ;
    (* then *)
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length numbers)

let union_set__when_same_set__then_no_changes =
  "union_set__when_same_set__then_no_changes" >:: fun _ ->
    (* given *)
    let test_object = IntSets.of_list [absent; numbers]
    and element1 = List.nth numbers 1
    and element2 = List.nth numbers 2 in
    (* when *)
    IntSets.union_set element1 element2 test_object ;
    (* then *)
    assert_that (IntSets.is_same_set element1 element2 test_object) Is.true_ ;
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to 2

let union_set__when_different_sets_in_chain__then_same_represent =
  "union_set__when_new_elements_in_chain__then_same_represent" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let rec union_all elems =
      match elems with
      | [] | [_] -> ()
      | x1 :: (x2 :: _ as xs) ->
        IntSets.union_set x1 x2 test_object ;
        union_all xs
    in
    union_all present ;
    (* then *)
    let first = List.hd present and last = List.hd @@ List.rev present in
    assert_that (IntSets.is_same_set first last test_object) Is.true_ ;
    assert_that (IntSets.find_set last test_object)
    @@ Is.Int.equal_to (IntSets.find_set first test_object) ;
    assert_that (IntSets.size test_object)
    @@ Is.Int.equal_to (List.length numbers - List.length present + 1)

let union_set_Test_list =
  test_list
    [ union_set__when_different_sets__then_same_represent;
      union_set__when_single_element__then_no_changes;
      union_set__when_same_set__then_no_changes;
      union_set__when_different_sets_in_chain__then_same_represent ]

(* is_same_set_Test_list *)

let is_same_set__when_different_sets__then_false =
  "is_same_set__when_different_sets__then_false" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () in
    (* when *)
    let result = IntSets.is_same_set (List.nth present 0) (List.nth present 1) test_object in
    (* then *)
    assert_that result Is.false_

let is_same_set__when_same_element__then_true =
  "is_same_set__when_same_element__then_true" >:: fun _ ->
    (* given *)
    let test_object = of_numbers () and element = List.hd present in
    (* when *)
    let result = IntSets.is_same_set element element test_object in
    (* then *)
    assert_that result Is.true_

let is_same_set__when_same_set__then_true =
  "is_same_set__when_same_set__then_true" >:: fun _ ->
    (* given *)
    let test_object = of_numbers ()
    and element1 = List.nth present 0
    and element2 = List.nth present 1 in
    IntSets.union_set element1 element2 test_object ;
    (* when *)
    let result = IntSets.is_same_set element2 element1 test_object in
    (* then *)
    assert_that result Is.true_

let is_same_set_Test_list =
  test_list
    [ is_same_set__when_different_sets__then_false;
      is_same_set__when_same_element__then_true;
      is_same_set__when_same_set__then_true ]

(* of_seq_Test_list *)

let of_seq__when_duplicates_in_different_sets__then_duplicate_elements =
  "of_seq__when_duplicates_in_different_sets__then_duplicate_elements" >:: fun _ ->
    (* given *)
    let sets =
      Seq.cons (Seq.cons 1 @@ Seq.cons 2 @@ Seq.return 3)
      @@ Seq.return (Seq.cons 1 @@ Seq.cons 11 @@ Seq.cons 21 @@ Seq.return 31)
    in
    (* when *)
    let exec () = IntSets.of_seq sets in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Duplicate_elements [1])

let of_seq__when_duplicates_in_same_set__then_created =
  "of_seq__when_duplicates_in_same_set__then_created" >:: fun _ ->
    (* given *)
    let sets =
      Seq.cons (Seq.cons 1 @@ Seq.cons 2 @@ Seq.return 3)
      @@ Seq.return (Seq.cons 10 @@ Seq.cons 100 @@ Seq.return 10)
    in
    (* when *)
    let test_object = IntSets.of_seq sets in
    (* then *)
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (Seq.length sets)

let of_seq_Test_list =
  test_list
    [ of_seq__when_duplicates_in_different_sets__then_duplicate_elements;
      of_seq__when_duplicates_in_same_set__then_created ]

(* of_list_Test_list *)

let of_list__when_duplicates_in_different_sets__then_duplicate_elements =
  "of_list__when_duplicates_in_different_sets__then_duplicate_elements" >:: fun _ ->
    (* when *)
    let exec () = IntSets.of_list [[1; 2; 3]; [1; 11; 21; 31]] in
    (* then *)
    assert_that exec @@ Is.raising (IntSets.Duplicate_elements [1])

let of_list__when_duplicates_in_same_set__then_created =
  "of_list__when_duplicates_in_same_set__then_created" >:: fun _ ->
    (* given *)
    let sets = [[1; 2; 3]; [10; 100; 10]] in
    (* when *)
    let test_object = IntSets.of_list sets in
    (* then *)
    assert_that (IntSets.size test_object) @@ Is.Int.equal_to (List.length sets)

let of_list_Test_list =
  test_list
    [ of_list__when_duplicates_in_different_sets__then_duplicate_elements;
      of_list__when_duplicates_in_same_set__then_created ]

(* disjoint_sets_Test *)

let disjoint_sets_Test =
  "Tests: Disjoint sets structure (union-find)"
  >::: [ size_Test_list;
         contains_Test_list;
         add_seq_Test_list;
         add_list_Test_list;
         find_set_Test_list;
         find_set_opt_Test_list;
         union_set_Test_list;
         is_same_set_Test_list;
         of_seq_Test_list;
         of_list_Test_list ]

let _ = run_test_tt_main disjoint_sets_Test
