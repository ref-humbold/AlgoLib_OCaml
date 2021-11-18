(* Tests: Algorithm for convex hull (monotone chain) in 2D *)
open OUnit2
open Algolib.Point_2d
open Algolib.Convex_hull
open Test_utils

let print_point (Point2D (x, y)) = "Point2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(* find_convex_hull_Test_list *)

let find_convex_hull__when_one_point__then_empty =
  "find_convex_hull When one point Then empty"
  >:: fun _ ->
    (* when *)
    let result = find_convex_hull [pt2d_i 3 2] in
    (* then *)
    assert_equal ~printer:(Printers.list print_point) [] result

let find_convex_hull__when_two_points__then_empty =
  "find_convex_hull When two points Then empty"
  >:: fun _ ->
    (* when *)
    let result = find_convex_hull [pt2d_i 2 3; pt2d_i 3 2] in
    (* then *)
    assert_equal ~printer:(Printers.list print_point) [] result

let find_convex_hull__when_three_points__then_these_points_are_hull =
  "find_convex_hull When three points Then these points are hull"
  >:: fun _ ->
    (* given *)
    let points = [pt2d_i 1 (-1); pt2d_i 5 1; pt2d_i 3 4] in
    (* when *)
    let result = find_convex_hull points in
    (* then *)
    assert_equal ~printer:(Printers.list print_point) points result

let find_convex_hull__then_points_in_hull =
  "find_convex_hull Then points in hull"
  >:: fun _ ->
    (* when *)
    let result =
      find_convex_hull
        [ pt2d_i 1 (-3); pt2d_i (-4) 6; pt2d_i (-5) (-7); pt2d_i (-8) (-7); pt2d_i (-3) (-4);
          pt2d_i 5 9; pt2d_i (-1) (-8); pt2d_i (-5) 10; pt2d_i 8 0; pt2d_i 3 (-6); pt2d_i (-2) 1;
          pt2d_i (-2) 8; pt2d_i 10 2; pt2d_i 6 3; pt2d_i (-7) 7; pt2d_i 6 (-4) ]
    in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [ pt2d_i (-8) (-7); pt2d_i (-1) (-8); pt2d_i 3 (-6); pt2d_i 6 (-4); pt2d_i 10 2; pt2d_i 5 9;
        pt2d_i (-5) 10; pt2d_i (-7) 7 ]
      result

let find_convex_hull__when_multiple_points_are_collinear__then_inner_points_omitted =
  "find_convex_hull When multiple points are collinear Then inner points omitted"
  >:: fun _ ->
    (* when *)
    let result =
      find_convex_hull
        [ pt2d_i (-1) (-3); pt2d_i (-3) (-3); pt2d_i (-3) (-1); pt2d_i 2 (-3); pt2d_i (-3) 5;
          pt2d_i 0 (-3); pt2d_i 7 (-3); pt2d_i (-3) (-2) ]
    in
    (* then *)
    assert_equal
      ~printer:(Printers.list print_point)
      [pt2d_i (-3) (-3); pt2d_i 7 (-3); pt2d_i (-3) 5]
      result

let find_convex_hull_Test_list =
  test_list
    [ find_convex_hull__when_one_point__then_empty; find_convex_hull__when_two_points__then_empty;
      find_convex_hull__when_three_points__then_these_points_are_hull;
      find_convex_hull__then_points_in_hull;
      find_convex_hull__when_multiple_points_are_collinear__then_inner_points_omitted ]

(* find_convex_hull_Test *)

let find_convex_hull_Test =
  "Tests: Algorithm for convex hull in 2D (monotone chain)" >::: [find_convex_hull_Test_list]

let _ = run_test_tt_main find_convex_hull_Test
