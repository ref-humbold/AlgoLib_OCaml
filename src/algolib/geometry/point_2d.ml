(* Points on a plane. *)
type point2d = float * float

let angle (x, y) =
  if y >= 0.0 then atan2 y x *. 45.0 /. atan 1.0 else (atan2 y x *. 45.0 /. atan 1.0) +. 360.0

let radius (x, y) = (x *. x) +. (y *. y)
