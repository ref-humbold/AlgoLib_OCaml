(* Structure of vector in a space *)
type vector3d = Vec3D of float * float * float

let vec3d x y z = Vec3D (x, y, z)

let vec3d_i x y z = Vec3D (float_of_int x, float_of_int y, float_of_int z)

let length (Vec3D (x, y, z)) = sqrt ((x *. x) +. (y *. y) +. (z *. z))

let ( +$ ) (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = Vec3D (x1 +. x2, y1 +. y2, z1 +. z2)

let ( -$ ) (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = Vec3D (x1 -. x2, y1 -. y2, z1 -. z2)

let ( *$ ) (Vec3D (x, y, z)) c = Vec3D (x *. c, y *. c, z *. c)

let ( /$ ) (Vec3D (x, y, z)) c =
  if c = 0.0 then raise Division_by_zero else Vec3D (x /. c, y /. c, z /. c)

let dot (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let cross (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) =
  Vec3D ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

let area v1 v2 = length @@ cross v1 v2

let volume v1 v2 v3 = dot v1 @@ cross v2 v3
