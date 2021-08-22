(* Tests: Algorithms for basic geometrical computations in 2D *)
open OUnit2
open Algolib
open Algolib.Geometry_2d
open Utils

let print_point (Point_2d.Point2D (x, y)) =
  "Point2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(* sort_by_x_Test_list *)

let sort_by_x__then_sorted_stably_ascending =
  "sort_by_x Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i (-2) (-3); Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i 2 3;
        Point_2d.pt2d_i 3 (-2); Point_2d.pt2d_i (-2) 3; Point_2d.pt2d_i 3 2; Point_2d.pt2d_i 2 (-3);
        Point_2d.pt2d_i (-3) (-2) ]
    in
    (* when *)
    let result = sort_by_x sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i (-3) (-2); Point_2d.pt2d_i (-2) (-3);
        Point_2d.pt2d_i (-2) 3; Point_2d.pt2d_i 0 0; Point_2d.pt2d_i 2 3; Point_2d.pt2d_i 2 (-3);
        Point_2d.pt2d_i 3 (-2); Point_2d.pt2d_i 3 2 ]
      result

let sort_by_x_Test_list = test_list [sort_by_x__then_sorted_stably_ascending]

(* sort_by_y_Test_list *)

let sort_by_y__then_sorted_stably_ascending =
  "sort_by_y Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i (-2) (-3); Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i 2 3;
        Point_2d.pt2d_i 3 (-2); Point_2d.pt2d_i (-2) 3; Point_2d.pt2d_i 3 2; Point_2d.pt2d_i 2 (-3);
        Point_2d.pt2d_i (-3) (-2) ]
    in
    (* when *)
    let result = sort_by_y sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_2d.pt2d_i (-2) (-3); Point_2d.pt2d_i 2 (-3); Point_2d.pt2d_i 3 (-2);
        Point_2d.pt2d_i (-3) (-2); Point_2d.pt2d_i 0 0; Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i 3 2;
        Point_2d.pt2d_i 2 3; Point_2d.pt2d_i (-2) 3 ]
      result

let sort_by_y_Test_list = test_list [sort_by_y__then_sorted_stably_ascending]

(* sort_by_angle_Test_list *)

let sort_by_angle__then_sorted_ascending =
  "sort_by_angle Then sorted stably ascending"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i (-2) (-3); Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i 2 3;
        Point_2d.pt2d_i 3 (-2); Point_2d.pt2d_i (-2) 3; Point_2d.pt2d_i 3 2; Point_2d.pt2d_i 2 (-3);
        Point_2d.pt2d_i (-3) (-2) ]
    in
    (* when *)
    let result = sort_by_angle sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i 3 2; Point_2d.pt2d_i 2 3; Point_2d.pt2d_i (-2) 3;
        Point_2d.pt2d_i (-3) 2; Point_2d.pt2d_i (-3) (-2); Point_2d.pt2d_i (-2) (-3);
        Point_2d.pt2d_i 2 (-3); Point_2d.pt2d_i 3 (-2) ]
      result

let sort_by_angle__when_equal_angles__then_compare_radius =
  "sort_by_angle When equal angles Then compare radius"
  >:: fun _ ->
    (* given *)
    let sequence =
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i 1 1; Point_2d.pt2d_i (-2) (-2);
        Point_2d.pt2d_i (-3) (-3); Point_2d.pt2d_i 4 4 ]
    in
    (* when *)
    let result = sort_by_angle sequence in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ Point_2d.pt2d_i 0 0; Point_2d.pt2d_i 1 1; Point_2d.pt2d_i 4 4; Point_2d.pt2d_i (-2) (-2);
        Point_2d.pt2d_i (-3) (-3) ]
      result

let sort_by_angle_Test_list =
  test_list
    [sort_by_angle__then_sorted_ascending; sort_by_angle__when_equal_angles__then_compare_radius]

(* distance_Test_list *)

let distance__when_different_points__then_distance =
  "distance When different points Then distance"
  >:: fun _ ->
    (* when *)
    let result = distance (Point_2d.pt2d_i 4 5) (Point_2d.pt2d_i (-2) (-3)) in
    (* then *)
    assert_equal ~printer:string_of_float 10.0 result

let distance__when_same_point__then_zero =
  "distance When same point Then zero"
  >:: fun _ ->
    (* given *)
    let point = Point_2d.pt2d 13.5 6.5 in
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
    let result = translate (Point_2d.pt2d 13.7 6.5) (Vector_2d.vec2d (-10.4) 3.3) in
    (* then *)
    assert_equal ~cmp:Point_2d.equal ~printer:print_point (Point_2d.pt2d 3.3 9.8) result

let translate__when_zero_vector__then_same_point =
  "translate When zero vector Then same point"
  >:: fun _ ->
    (* given *)
    let point = Point_2d.pt2d 13.5 6.5 in
    (* when *)
    let result = translate point (Vector_2d.vec2d_i 0 0) in
    (* then *)
    assert_equal ~cmp:Point_2d.equal ~printer:print_point point result

let translate_Test_list =
  test_list [translate__then_point_translated; translate__when_zero_vector__then_same_point]

(* geometry_2d_Test *)

let geometry_2d_Test =
  "Tests: Algorithms for basic geometrical computations in 2D"
  >::: [ sort_by_x_Test_list; sort_by_y_Test_list; sort_by_angle_Test_list; distance_Test_list;
         translate_Test_list ]

let _ = run_test_tt_main geometry_2d_Test
