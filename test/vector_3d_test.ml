(* Tests: Structure of vector in 3D *)
open OUnit2
open Algolib
open Algolib.Vector_3d
open Utils

let print_vector (Vector3D (x, y, z)) =
  "Vector3D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ", " ^ string_of_float z ^ ")"

let float_cmp = cmp_float ~epsilon:Geometry_object.epsilon

(* methods_Test_list *)

let between__then_vector_from_begin_to_end =
  "between Then vector from begin to end"
  >:: fun _ ->
    (* when *)
    let result = between (Point_3d.pt3d 2.4 7.8 (-10.3)) (Point_3d.pt3d (-1.5) 13.2 15.8) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d (-3.9) 5.4 26.1) result

let coordinates__then_triple_of_coordinates =
  "coordinates Then triple of coordinates"
  >:: fun _ ->
    (* when *)
    let result = coordinates @@ vec3d 5.0 (-19.0) 14.2 in
    (* then *)
    assert_equal
      ~printer:(Printers.tuple3 string_of_float string_of_float string_of_float)
      (5.0, -19.0, 14.2)
      result

let coordinates_list__then_list_of_coordinates =
  "coordinates list Then list of coordinates"
  >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ vec3d 5.0 (-19.0) 14.2 in
    (* then *)
    assert_equal ~printer:(Printers.list string_of_float) [5.0; -19.0; 14.2] result

let length__then_length_of_vector =
  "length Then length of vector"
  >:: fun _ ->
    (* when *)
    let result = length @@ vec3d_i 18 (-6) 13 in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 23.0 result

let dot__then_scalar_product =
  "dot Then scalar product"
  >:: fun _ ->
    (* when *)
    let result = dot (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float (-6.25) result

let dot__when_orthogonal__then_zero =
  "dot When orthogonal Then zero"
  >:: fun _ ->
    (* when *)
    let result = dot (vec3d_i 1 0 1) (vec3d_i 0 (-2) 0) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let cross__then_cross_product =
  "cross Then cross product"
  >:: fun _ ->
    (* when *)
    let result = cross (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d (-42.75) (-44.25) 32.25) result

let cross__when_parallel__then_zero =
  "cross When parallel Then zero"
  >:: fun _ ->
    (* when *)
    let result = cross (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d_i 0 0 0) result

let area__then_length_of_cross_product =
  "area Then length of cross product"
  >:: fun _ ->
    (* when *)
    let result = area (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 69.46716850426538 result

let area__when_parallel__then_zero =
  "area When parallel Then zero"
  >:: fun _ ->
    (* when *)
    let result = area (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let volume__then_scalar_triple_product =
  "volume Then scalar triple product"
  >:: fun _ ->
    (* when *)
    let result = volume (vec3d 1.5 (-4.0) (-3.5)) (vec3d 9.0 (-2.5) 8.5) (vec3d_i 1 (-1) 1) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 33.75 result

let volume__when_parallel__then_zero =
  "volume When parallel Then zero"
  >:: fun _ ->
    (* when *)
    let result = volume (vec3d_i 3 3 3) (vec3d_i (-8) (-8) (-8)) (vec3d_i 2 (-2) 2) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let volume__when_orthogonal__then_zero =
  "volume When orthogonal Then zero"
  >:: fun _ ->
    (* when *)
    let result = volume (vec3d_i 3 3 3) (vec3d_i 1 0 1) (vec3d_i 0 (-2) 0) in
    (* then *)
    assert_equal ~cmp:float_cmp ~printer:string_of_float 0.0 result

let methods_Test_list =
  test_list
    [ between__then_vector_from_begin_to_end; coordinates__then_triple_of_coordinates;
      coordinates_list__then_list_of_coordinates; length__then_length_of_vector;
      dot__then_scalar_product; dot__when_orthogonal__then_zero; cross__then_cross_product;
      cross__when_parallel__then_zero; area__then_length_of_cross_product;
      area__when_parallel__then_zero; volume__then_scalar_triple_product;
      volume__when_parallel__then_zero; volume__when_orthogonal__then_zero ]

(* operators_Test_list *)

let op_tilde_minus_dollar__then_negate_each_coordinate =
  "op tilde minus dollar Then negate each coordinate"
  >:: fun _ ->
    (* when *)
    let result = ~-$(vec3d 5.4 9.0 (-12.3)) in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d (-5.4) (-9.0) 12.3) result

let op_plus_dollar__then_add_each_coordinate =
  "op plus dollar Then add each coordinate"
  >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) +$ vec3d 7.9 (-8.1) 1.4 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d 13.3 0.9 (-10.9)) result

let op_minus_dollar__then_subtract_each_coordinate =
  "op minus dollar Then subtract each coordinate"
  >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) -$ vec3d 7.9 (-8.1) 1.4 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d (-2.5) 17.1 (-13.7)) result

let op_asterisk_dollar__then_multiply_each_coordinate =
  "op asterisk dollar Then multiply each coordinate"
  >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) *$ 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d 16.2 27.0 (-36.9)) result

let op_asterisk_dollar__when_multiplication_by_zero__then_zero_vector =
  "op asterisk dollar When multiplication by zero Then zero vector"
  >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) *$ 0.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d_i 0 0 0) result

let op_slash_dollar__then_divide_each_coordinate =
  "op slash dollar Then divide each coordinate"
  >:: fun _ ->
    (* when *)
    let result = vec3d 5.4 9.0 (-12.3) /$ 3.0 in
    (* then *)
    assert_equal ~cmp:equal ~printer:print_vector (vec3d 1.8 3.0 (-4.1)) result

let op_slash_dollar__when_division_by_zero__then_division_by_zero =
  "op slash dollar When division by zero Then Division by zero"
  >:: fun _ ->
    (* when *)
    let exec () = vec3d_i 1 1 1 /$ 0.0 in
    (* then *)
    assert_raises Division_by_zero exec

let operators_Test_list =
  test_list
    [ op_tilde_minus_dollar__then_negate_each_coordinate; op_plus_dollar__then_add_each_coordinate;
      op_minus_dollar__then_subtract_each_coordinate;
      op_asterisk_dollar__then_multiply_each_coordinate;
      op_asterisk_dollar__when_multiplication_by_zero__then_zero_vector;
      op_slash_dollar__then_divide_each_coordinate;
      op_slash_dollar__when_division_by_zero__then_division_by_zero ]

(* vector_3d_Test *)

let vector_3d_Test = "Tests: Structure of vector in 3D" >::: [methods_Test_list; operators_Test_list]

let _ = run_test_tt_main vector_3d_Test
