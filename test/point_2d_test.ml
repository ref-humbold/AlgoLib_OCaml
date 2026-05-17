(* Tests: Structure of point in 2D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim2.Point_2d
module A = Algolib.Geometry.Dim2.Angle_2d
module IsFloatPair = Is.Tuple2.Of (Values.Float) (Values.Float)
module IsFloatList = Is.List.Of (Values.Float)
module IsAngle = Is.TypeOf (A)

let epsilon = 1e-12

let params_for__angle =
  [ (zero, 0.0);
    (pt2d_i 7 0, 0.0);
    (pt2d_i 7 7, 45.0);
    (pt2d_i 0 7, 90.0);
    (pt2d_i (-7) 7, 135.0);
    (pt2d_i (-7) 0, 180.0);
    (pt2d_i (-7) (-7), 225.0);
    (pt2d_i 0 (-7), 270.0);
    (pt2d_i 7 (-7), 315.0) ]

let params_for__radius =
  [ (zero, 0.0);
    (pt2d_i 14 0, 14.0);
    (pt2d_i (-14) 0, 14.0);
    (pt2d_i 0 14, 14.0);
    (pt2d_i 0 (-14), 14.0);
    (pt2d_i 8 6, 10.0);
    (pt2d_i 8 (-6), 10.0);
    (pt2d_i (-8) 6, 10.0);
    (pt2d_i (-8) (-6), 10.0) ]

(* point2d_Test_list *)

let coordinates__then_tuple =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates @@ pt2d 150.123456789 (-3700.987654321) in
    (* then *)
    assert_that result @@ IsFloatPair.equal_to (150.123456789, -3700.987654321)

let coordinates_list__then_list =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = coordinates_list @@ pt2d 150.123456789 (-3700.987654321) in
    (* then *)
    assert_that result @@ IsFloatList.equal_to [150.123456789; -3700.987654321]

let angle__then_counter_clockwise_angle_from_x_axis =
  let with_param (param, expected) =
    let label = Printf.sprintf "%s %s" __FUNCTION__ (to_string param) in
    label >:: fun _ ->
      (* when *)
      let result = angle param in
      (* then *)
      assert_that result @@ IsAngle.equal_to @@ A.from_degrees expected
  in
  test_list @@ List.map with_param params_for__angle

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
    let result = to_string @@ pt2d 150.123456789 (-3700.987654321) in
    (* then *)
    assert_that result @@ Is.String.equal_to "(150.123456789, -3700.987654321)"

let point2d_Test_list =
  test_list
    [ coordinates__then_tuple;
      coordinates_list__then_list;
      angle__then_counter_clockwise_angle_from_x_axis;
      radius__then_distance_from_zero_point;
      to_string__then_string_representation ]

(* point_2d_Test *)

let point2d_Test = __MODULE__ >::: [point2d_Test_list]

let _ = run_test_tt_main point2d_Test
