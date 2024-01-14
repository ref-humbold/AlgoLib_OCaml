(* Tests: Structure of vector in 2D *)
open OUnit2
open Algolib.Vector_2d
open TestUtils

let print_vector (Vector2D (x, y)) =
  "Vector2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(* methods_Test_list *)

let between__then_vector_from_begin_to_end =
  "between Then vector from begin to end" >:: fun _ ->
    (* when *)
    let result = Algolib.Point_2d.(between (pt2d 2.4 7.8) (pt2d (-1.5) 13.2)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d (-3.9) 5.4) result

let coordinates__then_pair_of_coordinates =
  "coordinates Then pair of coordinates" >:: fun _ ->
    (* when *)
    let result = coordinates @@ vec2d_i 5 (-19) in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 string_of_float string_of_float) (5.0, -19.0) result

let coordinates_list__then_list_of_coordinates =
  "coordinates_list Then list of coordinates" >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ vec2d_i 5 (-19) in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_float) [5.0; -19.0] result

let length__then_length_of_vector =
  "length Then length of vector" >:: fun _ ->
    (* when *)
    let result = length @@ vec2d_i 8 (-6) in
    (* then *)
    Assert.Float.assert_close ~epsilon 10.0 result

let dot__then_scalar_product =
  "dot Then scalar product" >:: fun _ ->
    (* when *)
    let result = dot (vec2d 1.5 (-4.0)) (vec2d 9.0 (-2.5)) in
    (* then *)
    Assert.Float.assert_close ~epsilon 23.5 result

let dot__when_orthogonal__then_zero =
  "dot When orthogonal Then zero" >:: fun _ ->
    (* when *)
    let result = dot (vec2d_i 1 0) (vec2d_i 0 (-2)) in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

let area__then_length_of_cross_product =
  "area Then length of cross product" >:: fun _ ->
    (* when *)
    let result = area (vec2d 1.5 (-4.0)) (vec2d 9.0 (-2.5)) in
    (* then *)
    Assert.Float.assert_close ~epsilon 32.25 result

let area__when_parallel__then_zero =
  "area When parallel Then zero" >:: fun _ ->
    (* when *)
    let result = area (vec2d_i 3 3) (vec2d_i (-8) (-8)) in
    (* then *)
    Assert.Float.assert_close ~epsilon 0.0 result

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
  "op_tilde_colon Then negate each coordinate" >:: fun _ ->
    (* when *)
    let result = ~:(vec2d 5.4 9.0) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d (-5.4) (-9.0)) result

let op_plus_colon__then_add_each_coordinate =
  "op_plus_colon Then add each coordinate" >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 +: vec2d 7.9 (-8.1) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d 13.3 0.9) result

let op_minus_colon__then_subtract_each_coordinate =
  "op_minus_colon Then subtract each coordinate" >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 -: vec2d 7.9 (-8.1) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d (-2.5) 17.1) result

let op_asterisk_colon__then_multiply_each_coordinate =
  "op_asterisk_colon Then multiply each coordinate" >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 *: 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d 16.2 27.0) result

let op_asterisk_colon__when_multiplication_by_zero__then_zero_vector =
  "op_asterisk_colon When multiplication by zero Then zero vector" >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 *: 0.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d_i 0 0) result

let op_slash_colon__then_divide_each_coordinate =
  "op_slash_colon Then divide each coordinate" >:: fun _ ->
    (* when *)
    let result = vec2d 5.4 9.0 /: 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec2d 1.8 3.0) result

let op_slash_colon__when_division_by_zero__then_division_by_zero =
  "op_slash_colon When division by zero Then Division_by_zero" >:: fun _ ->
    (* when *)
    let exec () = vec2d_i 1 1 /: 0.0 in
    (* then *)
    assert_raises Division_by_zero exec

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

let vector_2d_Test = "Tests: Structure of vector in 2D" >::: [methods_Test_list; operators_Test_list]

let _ = run_test_tt_main vector_2d_Test
