(* Structure of vector in 2D. *)
include Geometry_object

type t = Vector2D of float * float

let vec2d x y = Vector2D (x, y)

let vec2d_i x y = Vector2D (float_of_int x, float_of_int y)

let between (Point_2d.Point2D (p1x, p1y)) (Point_2d.Point2D (p2x, p2y)) =
  vec2d (p2x -. p1x) (p2y -. p1y)

let coordinates (Vector2D (x, y)) = (x, y)

let coordinates_list (Vector2D (x, y)) = [x; y]

let equal v1 v2 = coordinates_equal (coordinates_list v1) (coordinates_list v2)

let length (Vector2D (x, y)) = sqrt ((x *. x) +. (y *. y))

let ( ~: ) (Vector2D (x, y)) = Vector2D (-.x, -.y)

let ( +: ) (Vector2D (x1, y1)) (Vector2D (x2, y2)) = Vector2D (x1 +. x2, y1 +. y2)

let ( -: ) (Vector2D (x1, y1)) (Vector2D (x2, y2)) = Vector2D (x1 -. x2, y1 -. y2)

let ( *: ) (Vector2D (x, y)) c = Vector2D (x *. c, y *. c)

let ( /: ) (Vector2D (x, y)) c =
  if c = 0.0 then raise Division_by_zero else Vector2D (x /. c, y /. c)

let dot (Vector2D (x1, y1)) (Vector2D (x2, y2)) = (x1 *. x2) +. (y1 *. y2)

let area (Vector2D (x1, y1)) (Vector2D (x2, y2)) = (x1 *. y2) -. (x2 *. y1)

let to_string (Vector2D (x, y)) = Printf.sprintf "Vector2D(%F, %F)" x y
