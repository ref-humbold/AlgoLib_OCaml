(* Tests: Algorithm for pair of closest points in 2D. *)
open OUnit2
open OAssert
open Algolib.Geometry.Dim2.Point_2d
open Algolib.Geometry.Dim2.Closest_points
module IsPointPair = Is.Tuple2.Of (Algolib.Geometry.Dim2.Point_2d) (Algolib.Geometry.Dim2.Point_2d)

(* find_closest_points_Test_list *)

let find_closest_points__when_no_points__then_not_found =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let exec () = find_closest_points [] in
    (* then *)
    assert_that exec @@ Is.raising Not_found

let find_closest_points__when_one_point__then_this_point =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2] in
    (* then *)
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i 2 2, pt2d_i 2 2)

let find_closest_points__when_two_points__then_these_points =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 2 2; pt2d_i 4 4] in
    (* then *)
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i 2 2, pt2d_i 4 4)

let find_closest_points__when_three_points__then_pair_of_closest_points =
  __FUNCTION__ >:: fun _ ->
    (* when *)
    let result = find_closest_points [pt2d_i 3 2; pt2d_i 1 1; pt2d_i 7 0] in
    (* then *)
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i 1 1, pt2d_i 3 2)

let find_closest_points__when_multiple_points__then_pair_of_closest_points =
  __FUNCTION__ >:: fun _ ->
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
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i 1 0, pt2d_i 1 1)

let find_closest_points_When_all_linear_on_x_Then_pair_of_closest_points =
  __FUNCTION__ >:: fun _ ->
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
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i 14 (-3), pt2d_i 14 1)

let find_closest_points_When_all_linear_on_y_Then_pair_of_closest_points =
  __FUNCTION__ >:: fun _ ->
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
    assert_that result @@ IsPointPair.equal_to @@ (pt2d_i (-8) (-6), pt2d_i (-3) (-6))

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

let find_closest_points_Test = __MODULE__ >::: [find_closest_points_Test_list]

let _ = run_test_tt_main find_closest_points_Test
