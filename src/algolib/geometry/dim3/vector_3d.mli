include module type of Geometry_object

(** The type of vectors in 3D. *)
type t = Vector3D of float * float * float

val vec3d : float -> float -> float -> t
(** [vec3d x y z] creates a vector [[x, y, z]]. *)

val vec3d_i : int -> int -> int -> t
(** [vec3d_i x y z] creates a vector [[x, y, z]]. *)

val between : Point_3d.t -> Point_3d.t -> t
(** [between p1 p2] creates a vector from point [p1] to point [p2]. *)

val coordinates : t -> float * float * float
(** [coordinates v] returns tuple of vector coordinates. *)

val coordinates_list : t -> float list
(** [coordinates_list v] returns list of vector coordinates. *)

val equal : t -> t -> bool
(** [vector v1 v2] checks whether vectors [v1] and [v2] are equal. *)

val length : t -> float
(** [length v] computes length of vector [v]. *)

val ( ~: ) : t -> t
(** [~: v] negates vector [v]. *)

val ( +: ) : t -> t -> t
(** [v1 +: v2] adds vectors [v1] and [v2]. Left associative. *)

val ( -: ) : t -> t -> t
(** [v1 -: v2] subtracts vectors [v1] and [v2]. Left associative. *)

val ( *: ) : t -> float -> t
(** [v *: c] multiplies vector [v] by a scalar [c]. Left associative. *)

val ( /: ) : t -> float -> t
(** [v /: c] divides vector [v] by a scalar [c]. Left associative. *)

val dot : t -> t -> float
(** [dot v1 v2] counts the dot product of vectors [v1] and [v2]. *)

val cross : t -> t -> t
(** [cross v1 v2] counts the cross product of vectors [v1] and [v2]. *)

val area : t -> t -> float
(** [area v1 v2] counts the area of parallelogram determined by vectors [v1] and [v2]. *)

val volume : t -> t -> t -> float
(** [volume v1 v2 v3] counts the volume of parallelepiped determined by vectors [v1], [v2] and [v3].
*)

val to_string : t -> string
(** [to_string v] returns string representation of vector [v]. **)
