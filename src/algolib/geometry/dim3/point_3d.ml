(* Structure of point in 3D *)

type point3d = Point3D of float * float * float

let pt3d x y z = Point3D (x, y, z)

let pt3d_i x y z = Point3D (float_of_int x, float_of_int y, float_of_int z)

let coordinates (Point3D (x, y, z)) = (x, y, z)

let radius (Point3D (x, y, z)) = sqrt ((x *. x) +. (y *. y) +. (z *. z))
