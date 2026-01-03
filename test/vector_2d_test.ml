(* Tests: Structure of vector in 2D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim2.Vector_2d
module P = Algolib.Geometry.Dim2.Point_2d
module IsFloatPair = Is.Tuple2.Of (Values.Float) (Values.Float)
module IsFloatList = Is.List.Of (Values.Float)
module IsVector = Is.TypeOf (Algolib.Geometry.Dim2.Vector_2d)

(* methods_Test_list *)

let between__then_vector_from_begin_to_end =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = between (P.pt2d 2.4 7.8) (P.pt2d (-1.5) 13.2) in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d (-3.9) 5.4

let coordinates__then_pair_of_coordinates =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates @@ vec2d_i 5 (-19) in
    (* then *)
    assert_that result @@ IsFloatPair.equal_to (5.0, -19.0)

let coordinates_list__then_list_of_coordinates =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ vec2d_i 5 (-19) in
    (* then *)
    assert_that result @@ IsFloatList.equal_to [5.0; -19.0]

let length__then_length_of_vector =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = length @@ vec2d_i 8 (-6) in
    (* then *)
    assert_that result @@ Is.Float.close_to 10.0 ~diff:(Difference epsilon)

let dot__then_scalar_product =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = dot (vec2d 1.5 (-4.0)) (vec2d 9.0 (-2.5)) in
    (* then *)
    assert_that result @@ Is.Float.close_to 23.5 ~diff:(Difference epsilon)

let dot__when_orthogonal__then_zero =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = dot (vec2d_i 1 0) (vec2d_i 0 (-2)) in
    (* then *)
    assert_that result Is.Float.zero

let area__then_length_of_cross_product =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = area (vec2d 1.5 (-4.0)) (vec2d 9.0 (-2.5)) in
    (* then *)
    assert_that result @@ Is.Float.close_to 32.25 ~diff:(Difference epsilon)

let area__when_parallel__then_zero =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = area (vec2d_i 3 3) (vec2d_i (-8) (-8)) in
    (* then *)
    assert_that result Is.Float.zero

let methods_Test_list =
  test_list
    [ between__then_vector_from_begin_to_end;
      coordinates__then_pair_of_coordinates;
      coordinates_list__then_list_of_coordinates;
      length__then_length_of_vector;
      dot__then_scalar_product;
      dot__when_orthogonal__then_zero;
      area__then_length_of_cross_product;
      area__when_parallel__then_zero ]

(* operators_Test_list *)

let op_tilde_colon__then_negate_each_coordinate =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = ~:(vec2d 5.4 9.0) in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d (-5.4) (-9.0)

let op_plus_colon__then_add_each_coordinate =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 +: vec2d 7.9 (-8.1) in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d 13.3 0.9

let op_minus_colon__then_subtract_each_coordinate =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 -: vec2d 7.9 (-8.1) in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d (-2.5) 17.1

let op_asterisk_colon__then_multiply_each_coordinate =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 *: 3.0 in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d 16.2 27.0

let op_asterisk_colon__when_multiplication_by_zero__then_zero_vector =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 *: 0.0 in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d_i 0 0

let op_slash_colon__then_divide_each_coordinate =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 /: 3.0 in
    (* then *)
    assert_that result @@ IsVector.equal_to @@ vec2d 1.8 3.0

let op_slash_colon__when_division_by_zero__then_division_by_zero =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = vec2d_i 1 1 /: 0.0 in
    (* then *)
    assert_that exec @@ Is.raising Division_by_zero

let operators_Test_list =
  test_list
    [ op_tilde_colon__then_negate_each_coordinate;
      op_plus_colon__then_add_each_coordinate;
      op_minus_colon__then_subtract_each_coordinate;
      op_asterisk_colon__then_multiply_each_coordinate;
      op_asterisk_colon__when_multiplication_by_zero__then_zero_vector;
      op_slash_colon__then_divide_each_coordinate;
      op_slash_colon__when_division_by_zero__then_division_by_zero ]

(* vector_2d_Test *)

let vector_2d_Test = __MODULE__ >::: [methods_Test_list; operators_Test_list]

let _ = run_test_tt_main vector_2d_Test
