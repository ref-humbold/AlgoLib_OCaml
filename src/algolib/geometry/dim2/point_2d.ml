(* Structure of point in 2D. *)
include Geometry_object

type point2d = Point2D of float * float

let pt2d x y = Point2D (x, y)

let pt2d_i x y = Point2D (float_of_int x, float_of_int y)

let coordinates (Point2D (x, y)) = (x, y)

let coordinates_list (Point2D (x, y)) = [x; y]

let equal p1 p2 = coordinates_equal (coordinates_list p1) (coordinates_list p2)

let radius (Point2D (x, y)) = sqrt ((x *. x) +. (y *. y))

let angle_rad (Point2D (x, y)) = atan2 y x

let angle_deg (Point2D (_, y) as p) =
  if y >= 0.0 then angle_rad p *. 45.0 /. atan 1.0 else (angle_rad p *. 45.0 /. atan 1.0) +. 360.0
