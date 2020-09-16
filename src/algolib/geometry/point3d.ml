(* Structure of point in a space *)
type point3d = Pt3D of float * float * float

let pt3d x y z = Pt3D (x, y, z)

let pt3d_i x y z = Pt3D (float_of_int x, float_of_int y, float_of_int z)

let radius (Pt3D (x, y, z)) = sqrt ((x *. x) +. (y *. y) +. (z *. z))
