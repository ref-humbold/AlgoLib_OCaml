(* Structure of point in 2D. *)
open Angle_2d
open Geometry_comparator

type t = Point2D of float * float

let zero = Point2D (0.0, 0.0)

let pt2d x y = Point2D (x, y)

let pt2d_i x y = Point2D (float_of_int x, float_of_int y)

let coordinates (Point2D (x, y)) = (x, y)

let coordinates_list (Point2D (x, y)) = [x; y]

let equal p1 p2 =
  List.for_all (fun (c1, c2) -> GeometryComparator.compare c1 c2 = 0)
  @@ List.combine (coordinates_list p1) (coordinates_list p2)

let radius (Point2D (x, y)) = sqrt ((x *. x) +. (y *. y))

let angle (Point2D (x, y)) = angle_rad (atan2 y x)

let to_string (Point2D (x, y)) = Printf.sprintf "(%.15F, %.15F)" x y
