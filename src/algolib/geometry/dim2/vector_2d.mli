include module type of Geometry_object

(** The type of vectors in 2D. *)
type t = Vector2D of float * float

val vec2d : float -> float -> t
(** [vec2d x y] creates a vector [[x, y]]. *)

val vec2d_i : int -> int -> t
(** [vec2d_i x y] creates a vector [[x, y]]. *)

val between : Point_2d.t -> Point_2d.t -> t
(** [between p1 p2] creates a vector from point [p1] to point [p2]. *)

val coordinates : t -> float * float
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

val area : t -> t -> float
(** [area v1 v2] counts the area of parallelogram determined by vectors [v1] and [v2]. *)

val to_string : t -> string
(** [to_string v] returns string representation of vector [v]. **)
