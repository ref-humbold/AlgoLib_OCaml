(** Structure of point on a plane *)

(** The type of points on a plane. *)
type point2d = float * float

val p2d : float -> float -> point2d
(** [p2d x y] creates a point [(x, y)]. *)

val p2d_i : int -> int -> point2d
(** [p2d_i x y] creates a point [(x, y)]. *)

val angle : point2d -> float
(** [angle p] computes angle in degrees of point [p] between its radius and X axis. Value of the
    angle is between 0 inclusive and 360 exclusive. *)

val radius : point2d -> float
(** [radius p] computes distance of point [p] from zero point. *)
