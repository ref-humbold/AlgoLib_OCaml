(* Structure of point in 3D *)
include Geometry_object

type point3d = Point3D of float * float * float

let pt3d x y z = Point3D (x, y, z)

let pt3d_i x y z = Point3D (float_of_int x, float_of_int y, float_of_int z)

let coordinates (Point3D (x, y, z)) = (x, y, z)

let coordinates_list (Point3D (x, y, z)) = [x; y; z]

let equal p1 p2 = coordinates_equal (coordinates_list p1) (coordinates_list p2)

let radius (Point3D (x, y, z)) = sqrt ((x *. x) +. (y *. y) +. (z *. z))
