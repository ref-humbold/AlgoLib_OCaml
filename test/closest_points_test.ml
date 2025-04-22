(* Tests: Algorithm for pair of closest points in 2D. *)
open OUnit2
open Algolib.Geometry.Dim2.Point_2d
open Algolib.Geometry.Dim2.Closest_points
open TestUtils

let print_point (Point2D (x, y)) = "Point2D(" ^ string_of_float x ^ ", " ^ string_of_float y ^ ")"

(* find_closest_points_Test_list *)

let find_closest_points__when_no_points__then_not_found =
  "find_closest_points__when_no_points__then_not_found" >:: fun _ ->
    (* when *)
    let exec () = find_closest_points [] in
    (* then *)
    assert_raises Not_found exec

let find_closest_points__when_one_point__then_this_point =
  "find_closest_points__when_one_point__then_this_point" >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 2 2, pt2d_i 2 2) result

let find_closest_points__when_two_points__then_these_points =
  "find_closest_points__when_two_points__then_these_points" >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2; pt2d_i 4 4] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 2 2, pt2d_i 4 4) result

let find_closest_points__when_three_points__then_pair_of_closest_points =
  "find_closest_points__when_three_points__then_pair_of_closest_points" >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 3 2; pt2d_i 1 1; pt2d_i 7 0] in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 1 1, pt2d_i 3 2) result

let find_closest_points__when_multiple_points__then_pair_of_closest_points =
  "find_closest_points__when_multiple_points__then_pair_of_closest_points" >:: fun _ ->
    (* when *)
    let result =
      find_closest_points
        [ pt2d_i 1 1;
          pt2d_i (-2) 2;
          pt2d_i (-4) 4;
          pt2d_i 3 (-3);
          pt2d_i 0 (-5);
          pt2d_i 1 0;
          pt2d_i (-7) 2;
          pt2d_i 4 5 ]
    in
    (* then *)
    assert_equal ~printer:(Printers.tuple2 print_point print_point) (pt2d_i 1 0, pt2d_i 1 1) result

let find_closest_points_When_all_linear_on_x_Then_pair_of_closest_points =
  "find_closest_points_When_all_linear_on_x_Then_pair_of_closest_points" >:: fun _ ->
    (* when *)
    let result =
      find_closest_points
        [ pt2d_i 14 (-40);
          pt2d_i 14 (-3);
          pt2d_i 14 36;
          pt2d_i 14 7;
          pt2d_i 14 (-24);
          pt2d_i 14 1;
          pt2d_i 14 (-14);
          pt2d_i 14 19 ]
    in
    (* then *)
    assert_equal
      ~printer:(Printers.tuple2 print_point print_point)
      (pt2d_i 14 (-3), pt2d_i 14 1)
      result

let find_closest_points_When_all_linear_on_y_Then_pair_of_closest_points =
  "find_closest_points_When_all_linear_on_y_Then_pair_of_closest_points" >:: fun _ ->
    (* when *)
    let result =
      find_closest_points
        [ pt2d_i (-27) (-6);
          pt2d_i 13 (-6);
          pt2d_i (-8) (-6);
          pt2d_i 30 (-6);
          pt2d_i 6 (-6);
          pt2d_i (-15) (-6);
          pt2d_i (-3) (-6);
          pt2d_i 22 (-6) ]
    in
    (* then *)
    assert_equal
      ~printer:(Printers.tuple2 print_point print_point)
      (pt2d_i (-8) (-6), pt2d_i (-3) (-6))
      result

let find_closest_points_Test_list =
  test_list
    [ find_closest_points__when_no_points__then_not_found;
      find_closest_points__when_one_point__then_this_point;
      find_closest_points__when_two_points__then_these_points;
      find_closest_points__when_three_points__then_pair_of_closest_points;
      find_closest_points__when_multiple_points__then_pair_of_closest_points;
      find_closest_points_When_all_linear_on_x_Then_pair_of_closest_points;
      find_closest_points_When_all_linear_on_y_Then_pair_of_closest_points ]

(* find_closest_points_Test *)

let find_closest_points_Test =
  "Tests: Algorithm for pair of closest points in 2D" >::: [find_closest_points_Test_list]

let _ = run_test_tt_main find_closest_points_Test
