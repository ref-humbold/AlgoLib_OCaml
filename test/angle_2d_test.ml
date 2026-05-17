(* Tests: Structure of angle in 2D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim2.Angle_2d

let epsilon = 1e-12

let params_for__degrees__when_from_radians =
  [ (0.0, 0.0);
    (Float.pi /. 6.0, 30.0);
    (Float.pi /. 4.0, 45.0);
    (Float.pi /. 3.0, 60.0);
    (Float.pi /. 2.0, 90.0);
    (Float.pi, 180.0);
    (2.0 *. Float.pi, 0.0);
    (3.0 *. Float.pi, 180.0);
    (-.Float.pi /. 6.0, 330.0);
    (-.Float.pi /. 4.0, 315.0);
    (-.Float.pi /. 3.0, 300.0);
    (-.Float.pi /. 2.0, 270.0);
    (-.Float.pi, 180.0);
    (-2.0 *. Float.pi, 0.0);
    (-3.0 *. Float.pi, 180.0) ]

let params_for__radians__when_from_degrees =
  [ (0.0, 0.0);
    (30.0, Float.pi /. 6.0);
    (45.0, Float.pi /. 4.0);
    (60.0, Float.pi /. 3.0);
    (90.0, Float.pi /. 2.0);
    (180.0, Float.pi);
    (360.0, 0.0);
    (540.0, Float.pi);
    (-30.0, 11.0 *. Float.pi /. 6.0);
    (-45.0, 7.0 *. Float.pi /. 4.0);
    (-60.0, 5.0 *. Float.pi /. 3.0);
    (-90.0, 3.0 *. Float.pi /. 2.0);
    (-180.0, Float.pi);
    (-360.0, 0.0);
    (-540.0, Float.pi) ]

(* angle_2d_Test_list *)

let degrees__when_from_radians__then_positive_degrees_in_range =
  let with_param (param, expected) =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* given *)
      let angle = from_radians param in
      (* when *)
      let result = degrees angle in
      (* then *)
      assert_that result @@ Is.Float.close_to expected ~diff:(Difference epsilon)
  in
  test_list @@ List.map with_param params_for__degrees__when_from_radians

let radians__when_from_degrees__then_positive_radians_in_range =
  let with_param (param, expected) =
    let label = Printf.sprintf "%s %F" __FUNCTION__ param in
    label >:: fun _ ->
      (* given *)
      let angle = from_degrees param in
      (* when *)
      let result = radians angle in
      (* then *)
      assert_that result @@ Is.Float.close_to expected ~diff:(Difference epsilon)
  in
  test_list @@ List.map with_param params_for__radians__when_from_degrees

let to_string__then_string_representation =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let angle = from_degrees 150.123456789 in
    (* when *)
    let result = to_string angle in
    (* then *)
    assert_that result @@ Is.String.equal_to "Angle<150.123456789 deg>"

let angle_2d_Test_list =
  test_list
    [ degrees__when_from_radians__then_positive_degrees_in_range;
      radians__when_from_degrees__then_positive_radians_in_range;
      to_string__then_string_representation ]

(* angle_2d_Test *)

let angle_2d_Test = __MODULE__ >::: [angle_2d_Test_list]

let _ = run_test_tt_main angle_2d_Test
