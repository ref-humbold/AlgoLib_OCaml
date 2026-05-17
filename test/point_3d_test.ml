(* Tests: Structure of point in 3D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim3.Point_3d
module IsFloatTriple = Is.Tuple3.Of (Values.Float) (Values.Float) (Values.Float)
module IsFloatList = Is.List.Of (Values.Float)

let epsilon = 1e-12

let params_for__radius =
  [ (zero, 0.0);
    (pt3d_i 14 0 0, 14.0);
    (pt3d_i (-14) 0 0, 14.0);
    (pt3d_i 0 14 0, 14.0);
    (pt3d_i 0 (-14) 0, 14.0);
    (pt3d_i 0 0 14, 14.0);
    (pt3d_i 0 0 (-14), 14.0);
    (pt3d_i 8 6 0, 10.0);
    (pt3d_i 8 (-6) 0, 10.0);
    (pt3d_i (-8) 6 0, 10.0);
    (pt3d_i (-8) (-6) 0, 10.0);
    (pt3d_i 8 0 6, 10.0);
    (pt3d_i 8 0 (-6), 10.0);
    (pt3d_i (-8) 0 6, 10.0);
    (pt3d_i (-8) 0 (-6), 10.0);
    (pt3d_i 0 8 6, 10.0);
    (pt3d_i 0 8 (-6), 10.0);
    (pt3d_i 0 (-8) 6, 10.0);
    (pt3d_i 0 (-8) (-6), 10.0);
    (pt3d_i 18 6 13, 23.0);
    (pt3d_i 18 6 (-13), 23.0);
    (pt3d_i 18 (-6) 13, 23.0);
    (pt3d_i 18 (-6) (-13), 23.0);
    (pt3d_i (-18) 6 13, 23.0);
    (pt3d_i (-18) 6 (-13), 23.0);
    (pt3d_i (-18) (-6) 13, 23.0);
    (pt3d_i (-18) (-6) (-13), 23.0) ]

(* point3d_Test_list *)

let coordinates__then_tuple =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates @@ pt3d 150.123456789 (-3700.987654321) 0.55555555 in
    (* then *)
    assert_that result @@ IsFloatTriple.equal_to (150.123456789, -3700.987654321, 0.55555555)

let coordinates_list__then_list =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ pt3d 150.123456789 (-3700.987654321) 0.55555555 in
    (* then *)
    assert_that result @@ IsFloatList.equal_to [150.123456789; -3700.987654321; 0.55555555]

let radius__then_distance_from_zero_point =
  let with_param (param, expected) =
    let label = Printf.sprintf "%s %s" __FUNCTION__ (to_string param) in
    label >:: fun _ ->
      (* when *)
      let result = radius param in
      (* then *)
      assert_that result @@ Is.Float.close_to expected ~diff:(Difference epsilon)
  in
  test_list @@ List.map with_param params_for__radius

let to_string__then_string_representation =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = to_string @@ pt3d 150.123456789 (-3700.987654321) 0.55555555 in
    (* then *)
    assert_that result @@ Is.String.equal_to "(150.123456789, -3700.987654321, 0.55555555)"

let point3d_Test_list =
  test_list
    [ coordinates__then_tuple;
      coordinates_list__then_list;
      radius__then_distance_from_zero_point;
      to_string__then_string_representation ]

(* point_3d_Test *)

let point3d_Test = __MODULE__ >::: [point3d_Test_list]

let _ = run_test_tt_main point3d_Test
