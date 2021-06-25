(** Structure of vector in 2D *)

(** The type of vectors in 2D. *)
type vector2d = Vector2D of float * float

val vec2d : float -> float -> vector2d
(** [vec2d x y] creates a vector [\[x, y\]]. *)

val vec2d_i : int -> int -> vector2d
(** [vec2d_i x y] creates a vector [\[x, y\]]. *)

val between : Point_2d.point2d -> Point_2d.point2d -> vector2d
(** [between p1 p2] creates a vector between points [p1] and [p2] *)

val length : vector2d -> float
(** [length v] computes length of vector [v]. *)

val ( +$ ) : vector2d -> vector2d -> vector2d
(** [v1 +$ v2] adds vectors [v1] and [v2]. Left associative. *)

val ( -$ ) : vector2d -> vector2d -> vector2d
(** [v1 -$ v2] subtracts vectors [v1] and [v2]. Left associative. *)

val ( *$ ) : vector2d -> float -> vector2d
(** [v *$ c] multiplies each coefficient of vector [v] by a constant [c]. Left associative. *)

val ( /$ ) : vector2d -> float -> vector2d
(** [v1 /$ v2] divides each coefficient of vector [v] by a constant [c]. Left associative. *)

val dot : vector2d -> vector2d -> float
(** [dot v1 v2] counts the dot product of vectors [v1] and [v2]. *)

val area : vector2d -> vector2d -> float
(** [area v1 v2] counts the area of parallelogram determined by vectors [v1] and [v2]. *)
