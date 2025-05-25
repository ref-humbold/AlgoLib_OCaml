(* Tests: Algorithms for basic geometrical computations in 3D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim3.Point_3d
open Algolib.Geometry.Dim3.Geometry_3d
module V = Algolib.Geometry.Dim3.Vector_3d

module PointType = struct
  type t = point3d

  let to_string (Point3D (x, y, z)) = Printf.sprintf "Point3D(%f, %f, %f)" x y z

  let equal = equal
end

module IsList = Is.List.Of (PointType)
module IsPoint = Is.Type (PointType)

(* sort_by_x_Test_list *)

let sort_by_x__then_sorted_stably_ascending =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let sequence =
      [ pt3d_i 0 0 0;
        pt3d_i 2 3 (-5);
        pt3d_i (-2) (-3) 5;
        pt3d_i 2 (-3) (-5);
        pt3d_i (-2) (-3) (-5);
        pt3d_i 3 2 5;
        pt3d_i (-3) 2 5 ]
    in
    (* when *)
    let result = sort_by_x sequence in
    (* then *)
    assert_that result
    @@ IsList.equal_to
      [ pt3d_i (-3) 2 5;
        pt3d_i (-2) (-3) 5;
        pt3d_i (-2) (-3) (-5);
        pt3d_i 0 0 0;
        pt3d_i 2 3 (-5);
        pt3d_i 2 (-3) (-5);
        pt3d_i 3 2 5 ]

let sort_by_x_Test_list = test_list [sort_by_x__then_sorted_stably_ascending]

(* sort_by_y_Test_list *)

let sort_by_y__then_sorted_stably_ascending =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let sequence =
      [ pt3d_i 0 0 0;
        pt3d_i 2 3 (-5);
        pt3d_i (-2) (-3) 5;
        pt3d_i 2 (-3) (-5);
        pt3d_i (-2) (-3) (-5);
        pt3d_i 3 2 5;
        pt3d_i (-3) 2 5 ]
    in
    (* when *)
    let result = sort_by_y sequence in
    (* then *)
    assert_that result
    @@ IsList.equal_to
      [ pt3d_i (-2) (-3) 5;
        pt3d_i 2 (-3) (-5);
        pt3d_i (-2) (-3) (-5);
        pt3d_i 0 0 0;
        pt3d_i 3 2 5;
        pt3d_i (-3) 2 5;
        pt3d_i 2 3 (-5) ]

let sort_by_y_Test_list = test_list [sort_by_y__then_sorted_stably_ascending]

(* sort_by_z_Test_list *)
let sort_by_z__then_sorted_stably_ascending =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let sequence =
      [ pt3d_i 0 0 0;
        pt3d_i 2 3 (-5);
        pt3d_i (-2) (-3) 5;
        pt3d_i 2 (-3) (-5);
        pt3d_i (-2) (-3) (-5);
        pt3d_i 3 2 5;
        pt3d_i (-3) 2 5 ]
    in
    (* when *)
    let result = sort_by_z sequence in
    (* then *)
    assert_that result
    @@ IsList.equal_to
      [ pt3d_i 2 3 (-5);
        pt3d_i 2 (-3) (-5);
        pt3d_i (-2) (-3) (-5);
        pt3d_i 0 0 0;
        pt3d_i (-2) (-3) 5;
        pt3d_i 3 2 5;
        pt3d_i (-3) 2 5 ]

let sort_by_z_Test_list = test_list [sort_by_z__then_sorted_stably_ascending]

(* distance_Test_list *)
let distance__when_different_points__then_distance =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = distance (pt3d_i 4 8 5) (pt3d_i (-2) (-1) 3) in
    (* then *)
    assert_that result @@ Is.Float.close_to 11.0 ~diff:epsilon

let distance__when_same_point__then_zero =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let point = pt3d 13.5 6.5 (-4.2) in
    (* when *)
    let result = distance point point in
    (* then *)
    assert_that result Is.Float.zero

let distance_Test_list =
  test_list [distance__when_different_points__then_distance; distance__when_same_point__then_zero]

(* translate_Test_list *)

let translate__then_point_translated =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = translate (pt3d 13.7 6.5 (-4.2)) (V.vec3d (-10.4) 3.3 1.1) in
    (* then *)
    assert_that result @@ IsPoint.equal_to @@ pt3d 3.3 9.8 (-3.1)

let translate__when_zero_vector__then_same_point =
  __FUNCTION__ >:: fun _ ->
    (* given *)
    let point = pt3d 13.5 6.5 (-4.2) in
    (* when *)
    let result = translate point (V.vec3d_i 0 0 0) in
    (* then *)
    assert_that result @@ IsPoint.equal_to point

let translate_Test_list =
  test_list [translate__then_point_translated; translate__when_zero_vector__then_same_point]

(* geometry_3d_Test *)

let geometry_3d_Test =
  "Tests: Algorithms for basic geometrical computations in 3D"
  >::: [ sort_by_x_Test_list;
         sort_by_y_Test_list;
         sort_by_z_Test_list;
         distance_Test_list;
         translate_Test_list ]

let _ = run_test_tt_main geometry_3d_Test
