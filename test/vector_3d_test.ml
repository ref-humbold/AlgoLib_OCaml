(* Tests: Structure of vector in 3D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim3.Vector_3d
module P = Algolib.Geometry.Dim3.Point_3d

module VectorType = struct
  type t = vector3d

  let to_string (Vector3D (x, y, z)) = Printf.sprintf "Vector3D(%f, %f, %f)" x y z
end

module IsFloatList = Is.List.Of (Type.Float)

(* methods_Test_list *)

let between__then_vector_from_begin_to_end =
  "between__then_vector_from_begin_to_end" >:: fun _ ->
    (* when *)
    let result = between (P.pt3d 2.4 7.8 (-10.3)) (P.pt3d (-1.5) 13.2 15.8) in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d (-3.9) 5.4 26.1) result

let coordinates__then_triple_of_coordinates =
  "coordinates__then_triple_of_coordinates" >:: fun _ ->
    (* when *)
    let result = coordinates @@ vec3d 5.0 (-19.0) 14.2 in
    (* then *)
    assert_equal
      ~printer:(fun (e1, e2, e3) -> Printf.sprintf "(%f, %f, %f)" e1 e2 e3)
      (5.0, -19.0, 14.2)
      result

let coordinates_list__then_list_of_coordinates =
  "coordinates_list__then_list_of_coordinates" >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ vec3d 5.0 (-19.0) 14.2 in
    (* then *)
    assert_that result @@ IsFloatList.equal_to [5.0; -19.0; 14.2]

let length__then_length_of_vector =
  "length__then_length_of_vector" >:: fun _ ->
    (* when *)
    let result = length @@ vec3d_i 18 (-6) 13 in
    (* then *)
    assert_that result @@ Is.Float.close_to 23.0 ~diff:epsilon

let dot__then_scalar_product =
  "dot__then_scalar_product" >:: fun _ ->
    (* when *)
    let result = dot (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_that result @@ Is.Float.close_to (-6.25) ~diff:epsilon

let dot__when_orthogonal__then_zero =
  "dot__when_orthogonal__then_zero" >:: fun _ ->
    (* when *)
    let result = dot (vec3d_i 1 0 1) (vec3d_i 0 (-2) 0) in
    (* then *)
    assert_that result Is.Float.zero

let cross__then_cross_product =
  "cross__then_cross_product" >:: fun _ ->
    (* when *)
    let result = cross (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d (-42.75) (-44.25) 32.25) result

let cross__when_parallel__then_zero =
  "cross__when_parallel__then_zero" >:: fun _ ->
    (* when *)
    let result = cross (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d_i 0 0 0) result

let area__then_length_of_cross_product =
  "area__then_length_of_cross_product" >:: fun _ ->
    (* when *)
    let result = area (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_that result @@ Is.Float.close_to 69.46716850426538 ~diff:epsilon

let area__when_parallel__then_zero =
  "area__when_parallel__then_zero" >:: fun _ ->
    (* when *)
    let result = area (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) in
    (* then *)
    assert_that result Is.Float.zero

let volume__then_scalar_triple_product =
  "volume__then_scalar_triple_product" >:: fun _ ->
    (* when *)
    let result = volume (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) (vec3d_i 1 (-1) 1) in
    (* then *)
    assert_that result @@ Is.Float.close_to 33.75 ~diff:epsilon

let volume__when_parallel__then_zero =
  "volume__when_parallel__then_zero" >:: fun _ ->
    (* when *)
    let result = volume (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) (vec3d_i 2 (-2) 2) in
    (* then *)
    assert_that result Is.Float.zero

let volume__when_orthogonal__then_zero =
  "volume__when_orthogonal__then_zero" >:: fun _ ->
    (* when *)
    let result = volume (vec3d_i 3 3 3) (vec3d_i 1 0 1) (vec3d_i 0 (-2) 0) in
    (* then *)
    assert_that result Is.Float.zero

let methods_Test_list =
  test_list
    [ between__then_vector_from_begin_to_end;
      coordinates__then_triple_of_coordinates;
      coordinates_list__then_list_of_coordinates;
      length__then_length_of_vector;
      dot__then_scalar_product;
      dot__when_orthogonal__then_zero;
      cross__then_cross_product;
      cross__when_parallel__then_zero;
      area__then_length_of_cross_product;
      area__when_parallel__then_zero;
      volume__then_scalar_triple_product;
      volume__when_parallel__then_zero;
      volume__when_orthogonal__then_zero ]

(* operators_Test_list *)

let op_tilde_colon__then_negate_each_coordinate =
  "op_tilde_colon__then_negate_each_coordinate" >:: fun _ ->
    (* when *)
    let result = ~:(vec3d 5.4 9.0 (-12.3)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d (-5.4) (-9.0) 12.3) result

let op_plus_colon__then_add_each_coordinate =
  "op_plus_colon__then_add_each_coordinate" >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) +: vec3d 7.9 (-8.1) 1.4 in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d 13.3 0.9 (-10.9)) result

let op_minus_colon__then_subtract_each_coordinate =
  "op_minus_colon__then_subtract_each_coordinate" >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) -: vec3d 7.9 (-8.1) 1.4 in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d (-2.5) 17.1 (-13.7)) result

let op_asterisk_colon__then_multiply_each_coordinate =
  "op_asterisk_colon__then_multiply_each_coordinate" >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) *: 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d 16.2 27.0 (-36.9)) result

let op_asterisk_colon__when_multiplication_by_zero__then_zero_vector =
  "op_asterisk_colon__when_multiplication_by_zero__then_zero_vector" >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) *: 0.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d_i 0 0 0) result

let op_slash_colon__then_divide_each_coordinate =
  "op_slash_colon__then_divide_each_coordinate" >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) /: 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:VectorType.to_string (vec3d 1.8 3.0 (-4.1)) result

let op_slash_colon__when_division_by_zero__then_division_by_zero =
  "op_slash_colon__when_division_by_zero__then_division_by_zero" >:: fun _ ->
    (* when *)
    let exec () = vec3d_i 1 1 1 /: 0.0 in
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

(* vector_3d_Test *)

let vector_3d_Test = "Tests: Structure of vector in 3D" >::: [methods_Test_list; operators_Test_list]

let _ = run_test_tt_main vector_3d_Test
