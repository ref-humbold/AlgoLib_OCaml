(* Tests: Algorithms for basic geometrical computations in 3D *)
open OUnit2
open Algolib
open Algolib.Geometry_3d
open Utils

let print_point (Point_3d.Point3D (x, y, z)) =
  "Point3D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ", " ^ string_of_float z ^ ")"

(* sort_by_x_Test_list *)

let sort_by_x__then_sorted_stably_ascending =
  "sort_by_x Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_3d.Point3D (0.0, 0.0, 0.0); Point_3d.Point3D (2.0, 3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, 5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (3.0, 2.0, 5.0);
        Point_3d.Point3D (-3.0, 2.0, 5.0) ]
    in
    (* when *)
    let result = sort_by_x sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_3d.Point3D (-3.0, 2.0, 5.0); Point_3d.Point3D (-2.0, -3.0, 5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (0.0, 0.0, 0.0);
        Point_3d.Point3D (2.0, 3.0, -5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (3.0, 2.0, 5.0) ]
      result

let sort_by_x_Test_list = test_list [sort_by_x__then_sorted_stably_ascending]

(* sort_by_y_Test_list *)

let sort_by_y__then_sorted_stably_ascending =
  "sort_by_y Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_3d.Point3D (0.0, 0.0, 0.0); Point_3d.Point3D (2.0, 3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, 5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (3.0, 2.0, 5.0);
        Point_3d.Point3D (-3.0, 2.0, 5.0) ]
    in
    (* when *)
    let result = sort_by_y sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_3d.Point3D (-2.0, -3.0, 5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (0.0, 0.0, 0.0);
        Point_3d.Point3D (3.0, 2.0, 5.0); Point_3d.Point3D (-3.0, 2.0, 5.0);
        Point_3d.Point3D (2.0, 3.0, -5.0) ]
      result

let sort_by_y_Test_list = test_list [sort_by_y__then_sorted_stably_ascending]

(* sort_by_z_Test_list *)
let sort_by_z__then_sorted_stably_ascending =
  "sort_by_z Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_3d.Point3D (0.0, 0.0, 0.0); Point_3d.Point3D (2.0, 3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, 5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (3.0, 2.0, 5.0);
        Point_3d.Point3D (-3.0, 2.0, 5.0) ]
    in
    (* when *)
    let result = sort_by_z sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_3d.Point3D (2.0, 3.0, -5.0); Point_3d.Point3D (2.0, -3.0, -5.0);
        Point_3d.Point3D (-2.0, -3.0, -5.0); Point_3d.Point3D (0.0, 0.0, 0.0);
        Point_3d.Point3D (-2.0, -3.0, 5.0); Point_3d.Point3D (3.0, 2.0, 5.0);
        Point_3d.Point3D (-3.0, 2.0, 5.0) ]
      result

let sort_by_z_Test_list = test_list [sort_by_z__then_sorted_stably_ascending]

(* distance_Test_list *)
let distance__when_different_points__then_distance =
  "distance When different points Then distance"
  >:: fun _ ->
    (* when *)
    let result = distance (Point_3d.Point3D (4.0, 8.0, 5.0)) (Point_3d.Point3D (-2.0, -1.0, 3.0)) in
    (* then *)
    assert_equal ~printer:string_of_float 11.0 result

let distance__when_same_point__then_zero =
  "distance When same point Then zero"
  >:: fun _ ->
    (* given *)
    let point = Point_3d.Point3D (13.5, 6.5, -4.2) in
    (* when *)
    let result = distance point point in
    (* then *)
    assert_equal ~printer:string_of_float 0.0 result

let distance_Test_list =
  test_list [distance__when_different_points__then_distance; distance__when_same_point__then_zero]

(* translate_Test_list *)

let translate__then_point_translated =
  "translate Then point translated"
  >:: fun _ ->
    (* when *)
    let result =
      translate (Point_3d.Point3D (13.7, 6.5, -4.2)) (Vector_3d.Vector3D (-10.4, 3.3, 1.1))
    in
    (* then *)
    assert_equal ~cmp:Point_3d.equal ~printer:print_point (Point_3d.Point3D (3.3, 9.8, -3.1)) result

let translate__when_zero_vector__then_same_point =
  "translate When zero vector Then same point"
  >:: fun _ ->
    (* given *)
    let point = Point_3d.Point3D (13.5, 6.5, -4.2) in
    (* when *)
    let result = translate point (Vector_3d.Vector3D (0.0, 0.0, 0.0)) in
    (* then *)
    assert_equal ~cmp:Point_3d.equal ~printer:print_point point result

let translate_Test_list =
  test_list [translate__then_point_translated; translate__when_zero_vector__then_same_point]

(* geometry_3d_Test *)

let geometry_3d_Test =
  "Tests: Algorithms for basic geometrical computations in 3D"
  >::: [ sort_by_x_Test_list; sort_by_y_Test_list; sort_by_z_Test_list; distance_Test_list;
         translate_Test_list ]

let _ = run_test_tt_main geometry_3d_Test
