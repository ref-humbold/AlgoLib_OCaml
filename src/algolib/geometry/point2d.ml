(* Structure of point on a plane *)
type point2d = float * float

let p2d x y = (x, y)

let p2d_i x y = (float_of_int x, float_of_int y)

let angle (x, y) =
  if y >= 0.0 then atan2 y x *. 45.0 /. atan 1.0 else (atan2 y x *. 45.0 /. atan 1.0) +. 360.0

let radius (x, y) = (x *. x) +. (y *. y)
