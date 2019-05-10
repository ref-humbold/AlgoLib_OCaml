(** Points on a plane. *)

(** The type of points on a plane. *)
type point2d = float * float

val angle : point2d -> float
(** [angle p] computes angle in degrees of point [p] between its radius and X axis. Value of the
    angle is between 0 inclusive and 360 exclusive. *)

val radius : point2d -> float
(** [radius p] computes distance of point [p] from zero point. *)

val compare : point2d -> point2d -> int
(** [compare p1 p2] compares two points by their coordinates. First compares their X coordinate,
    then Y coordinate. *)
