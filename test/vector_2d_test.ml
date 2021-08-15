(* Tests: Structure of vector in 2D *)
open OUnit2
open Algolib
open Algolib.Vector_2d
open Utils

let print_vector (Vector2D (x, y)) =
  "Vector2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

let float_cmp = cmp_float ~epsilon:Geometry_object.epsilon

(* methods_Test_list *)

let between__then_vector_from_begin_to_end =
  "between Then vector from begin to end"
  >:: fun _ ->
    (* when *)
    let result = between (Point_2d.Point2D (2.4, 7.8)) (Point_2d.Point2D (-1.5, 13.2)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (-3.9, 5.4)) result

let coordinates__then_pair_of_coordinates =
  "coordinates Then pair of coordinates"
  >:: fun _ ->
    (* when *)
    let result = coordinates @@ Vector2D (5.0, -19.0) in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 string_of_float string_of_float) (5.0, -19.0) result

let length__then_length_of_vector =
  "length Then length of vector"
  >:: fun _ ->
    (* when *)
    let result = length @@ Vector2D (8.0, -6.0) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 10.0 result

let methods_Test_list =
  test_list
    [ between__then_vector_from_begin_to_end; coordinates__then_pair_of_coordinates;
      length__then_length_of_vector ]

(* operators_Test_list *)
let op_tilde_minus_dollar__then_negate_each_coordinate =
  "op_tilde_minus_dollar Then negate each coordinate"
  >:: fun _ ->
    (* when *)
    let result = ~-$(Vector2D (5.4, 9.0)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (-5.4, -9.0)) result

let op_plus_dollar__then_add_each_coordinate =
  "op_plus_dollar Then add each coordinate"
  >:: fun _ ->
    (* when *)
    let result = Vector2D (5.4, 9.0) +$ Vector2D (7.9, -8.1) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (13.3, 0.9)) result

let op_minus_dollar__then_subtract_each_coordinate =
  "op_minus_dollar Then subtract each coordinate"
  >:: fun _ ->
    (* when *)
    let result = Vector2D (5.4, 9.0) -$ Vector2D (7.9, -8.1) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (-2.5, 17.1)) result

let op_asterisk_dollar__then_multiply_each_coordinate =
  "op_asterisk_dollar Then multiply each coordinate"
  >:: fun _ ->
    (* when *)
    let result = Vector2D (5.4, 9.0) *$ 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (16.2, 27.0)) result

let op_asterisk_dollar__when_multiplication_by_zero__then_zero_vector =
  "op_asterisk_dollar When multiplication by zero Then zero vector"
  >:: fun _ ->
    (* when *)
    let result = Vector2D (5.4, 9.0) *$ 0.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (0.0, 0.0)) result

let op_slash_dollar__then_divide_each_coordinate =
  "op_slash_dollar Then divide each coordinate"
  >:: fun _ ->
    (* when *)
    let result = Vector2D (5.4, 9.0) /$ 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (Vector2D (1.8, 3.0)) result

let op_slash_dollar__when_division_by_zero__then_division_by_zero =
  "op_slash_dollar When division by zero Then Division_by_zero"
  >:: fun _ ->
    (* when *)
    let result () = Vector2D (1.0, 1.0) /$ 0.0 in
    (* then *)
    assert_raises Division_by_zero result

let operators_Test_list =
  test_list
    [ op_tilde_minus_dollar__then_negate_each_coordinate; op_plus_dollar__then_add_each_coordinate;
      op_minus_dollar__then_subtract_each_coordinate;
      op_asterisk_dollar__then_multiply_each_coordinate;
      op_asterisk_dollar__when_multiplication_by_zero__then_zero_vector;
      op_slash_dollar__then_divide_each_coordinate;
      op_slash_dollar__when_division_by_zero__then_division_by_zero ]

(* operations_Test_list *)

let dot__then_scalar_product =
  "dot Then scalar product"
  >:: fun _ ->
    (* when *)
    let result = dot (Vector2D (1.5, -4.0)) (Vector2D (9.0, -2.5)) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 23.5 result

let dot__when_orthogonal__then_zero =
  "dot When orthogonal Then zero"
  >:: fun _ ->
    (* when *)
    let result = dot (Vector2D (1.0, 0.0)) (Vector2D (0.0, -2.0)) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let area__then_length_of_cross_product =
  "area Then length of cross product"
  >:: fun _ ->
    (* when *)
    let result = area (Vector2D (1.5, -4.0)) (Vector2D (9.0, -2.5)) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 32.25 result

let area__when_parallel__then_zero =
  "area When parallel Then zero"
  >:: fun _ ->
    (* when *)
    let result = area (Vector2D (3.0, 3.0)) (Vector2D (-8.0, -8.0)) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let operations_Test_list =
  test_list
    [ dot__then_scalar_product; dot__when_orthogonal__then_zero; area__then_length_of_cross_product;
      area__when_parallel__then_zero ]

(* vector_2d_Test *)

let vector_2d_Test =
  "Tests: Structure of vector in 2D"
  >::: [methods_Test_list; operators_Test_list; operations_Test_list]

let _ = run_test_tt_main vector_2d_Test
