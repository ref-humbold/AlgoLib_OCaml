(* Structure of point on a plane *)
type point2d = Pt2D of float * float

let pt2d x y = Pt2D (x, y)

let pt2d_i x y = Pt2D (float_of_int x, float_of_int y)

let angle_rad (Pt2D (x, y)) = atan2 y x

let angle_deg (Pt2D (_, y) as p) =
  if y >= 0.0 then angle_rad p *. 45.0 /. atan 1.0 else (angle_rad p *. 45.0 /. atan 1.0) +. 360.0

let radius (Pt2D (x, y)) = sqrt ((x *. x) +. (y *. y))
