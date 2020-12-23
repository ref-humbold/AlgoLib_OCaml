(** Structure of vector in a space *)

(** The type of vectors in a space. *)
type vector3d = Vec3D of float * float * float

val vec3d : float -> float -> float -> vector3d
(** [vec3d x y z] creates a vector [\[x, y, z\]]. *)

val vec3d_i : int -> int -> int -> vector3d
(** [vec3d_i x y z] creates a vector [\[x, y, z\]]. *)

val length : vector3d -> float
(** [length v] computes length of vector [v]. *)

val ( +$ ) : vector3d -> vector3d -> vector3d
(** [v1 +$ v2] adds vectors [v1] and [v2]. Left associative. *)

val ( -$ ) : vector3d -> vector3d -> vector3d
(** [v1 -$ v2] subtracts vectors [v1] and [v2]. Left associative. *)

val ( *$ ) : vector3d -> float -> vector3d
(** [v *$ c] multiplies each coefficient of vector [v] by a constant [c]. Left associative. *)

val ( /$ ) : vector3d -> float -> vector3d
(** [v1 /$ v2] divides each coefficient of vector [v] by a constant [c]. Left associative. *)

val dot : vector3d -> vector3d -> float
(** [dot v1 v2] counts the dot product of vectors [v1] and [v2]. *)

val cross : vector3d -> vector3d -> vector3d
(** [cross v1 v2] counts the cross product of vectors [v1] and [v2]. *)

val area : vector3d -> vector3d -> float
(** [area v1 v2] counts the area of parallelogram determined by vectors [v1] and [v2]. *)

val volume : vector3d -> vector3d -> vector3d -> float
(** [volume v1 v2 v3] counts the volume of parallelepiped determined by vectors [v1], [v2] and [v3]. *)
