(* Structure of angle in 2D. *)
open Geometry_comparator

type t = Angle2D of float

let max_degrees = 360.0

let max_radians = 2.0 *. Float.pi

let from_degrees degrees =
  Angle2D (Float.rem (Float.rem degrees max_degrees +. max_degrees) max_degrees)

let from_radians radians = from_degrees (radians *. max_degrees /. max_radians)

let degrees (Angle2D deg) = deg

let radians (Angle2D deg) = deg *. max_radians /. max_degrees

let compare (Angle2D deg1) (Angle2D deg2) = GeometryComparator.compare deg1 deg2

let equal a1 a2 = compare a1 a2 = 0

let to_string (Angle2D deg) = Printf.sprintf "Angle<%.15F deg>" deg
