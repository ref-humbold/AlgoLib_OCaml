(** Structure of vector in 3D *)

(** The type of vectors in 3D. *)
type vector3d = Vector3D of float * float * float

val vec3d : float -> float -> float -> vector3d
(** [vec3d x y z] creates a vector [\[x, y, z\]]. *)

val vec3d_i : int -> int -> int -> vector3d
(** [vec3d_i x y z] creates a vector [\[x, y, z\]]. *)

val between : Point_3d.point3d -> Point_3d.point3d -> vector3d
(** [between p1 p2] creates a vector from point [p1] to point [p2]. *)

val coordinates : vector3d -> float * float * float
(** [coordeinates v] returns tuple of vector coordinates. *)

val length : vector3d -> float
(** [length v] computes length of vector [v]. *)

val ( ~-$ ) : vector3d -> vector3d
(** [~-$ v] negates vector [v]. *)

val ( +$ ) : vector3d -> vector3d -> vector3d
(** [v1 +$ v2] adds vectors [v1] and [v2]. Left associative. *)

val ( -$ ) : vector3d -> vector3d -> vector3d
(** [v1 -$ v2] subtracts vectors [v1] and [v2]. Left associative. *)

val ( *$ ) : vector3d -> float -> vector3d
(** [v *$ c] multiplies vector [v] by a scalar [c]. Left associative. *)

val ( /$ ) : vector3d -> float -> vector3d
(** [v1 /$ v2] divides vector [v] by a scalar [c]. Left associative. *)

val dot : vector3d -> vector3d -> float
(** [dot v1 v2] counts the dot product of vectors [v1] and [v2]. *)

val cross : vector3d -> vector3d -> vector3d
(** [cross v1 v2] counts the cross product of vectors [v1] and [v2]. *)

val area : vector3d -> vector3d -> float
(** [area v1 v2] counts the area of parallelogram determined by vectors [v1] and [v2]. *)

val volume : vector3d -> vector3d -> vector3d -> float
(** [volume v1 v2 v3] counts the volume of parallelepiped determined by vectors [v1], [v2] and [v3]. *)
