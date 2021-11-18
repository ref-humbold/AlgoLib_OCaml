(* Tests: Algorithm for convex hull (monotone chain) in 2D *)
open OUnit2
open Algolib.Point_2d
open Algolib.Closest_points
open Test_utils

let print_point (Point2D (x, y)) = "Point2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(* find_closest_points_Test_list *)

let find_closest_points__when_one_point__then_this_point =
  "find_closest_points When one point Then this point"
  >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 2 2, pt2d_i 2 2) result

let find_closest_points__when_two_points__then_these_points =
  "find_closest_points When two points Then these point"
  >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2; pt2d_i 4 4] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 2 2, pt2d_i 4 4) result

let find_closest_points__when_three_points__then_pair_of_closest_points =
  "find_closest_points When three points Then pair of closest points"
  >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 3 2; pt2d_i 1 1; pt2d_i 7 0] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 1 1, pt2d_i 3 2) result

let test__find_closest_points__when_multiple_points__then_pair_of_closest_points =
  "find_closest_points When multiple points Then pair of closest points"
  >:: fun _ ->
    (* when *)
    let result =
      find_closest_points
        [ pt2d_i 1 1; pt2d_i (-2) 2; pt2d_i (-4) 4; pt2d_i 3 (-3); pt2d_i 0 (-5); pt2d_i 1 0;
          pt2d_i (-7) 2; pt2d_i 4 5 ]
    in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 1 1, pt2d_i 1 0) result

let find_closest_points_Test_list =
  test_list
    [ find_closest_points__when_one_point__then_this_point;
      find_closest_points__when_two_points__then_these_points;
      find_closest_points__when_three_points__then_pair_of_closest_points;
      test__find_closest_points__when_multiple_points__then_pair_of_closest_points ]

(* find_closest_points_Test *)

let find_closest_points_Test =
  "Tests: Algorithm for pair of closest points in 2D" >::: [find_closest_points_Test_list]

let _ = run_test_tt_main find_closest_points_Test
