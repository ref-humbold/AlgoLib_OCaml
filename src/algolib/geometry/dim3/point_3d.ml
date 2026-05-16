(* Structure of point in 3D. *)
open Geometry_comparator

type t = Point3D of float * float * float

let pt3d x y z = Point3D (x, y, z)

let pt3d_i x y z = Point3D (float_of_int x, float_of_int y, float_of_int z)

let coordinates (Point3D (x, y, z)) = (x, y, z)

let coordinates_list (Point3D (x, y, z)) = [x; y; z]

let equal p1 p2 =
  List.for_all (fun (c1, c2) -> GeometryComparator.compare c1 c2 = 0)
  @@ List.combine (coordinates_list p1) (coordinates_list p2)

let radius (Point3D (x, y, z)) = sqrt ((x *. x) +. (y *. y) +. (z *. z))

let to_string (Point3D (x, y, z)) = Printf.sprintf "(%.15F, %.15F, %.15F)" x y z
