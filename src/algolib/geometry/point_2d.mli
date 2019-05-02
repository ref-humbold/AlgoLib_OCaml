(** Points on a plane. *)

type point = float * float
(** The type of points. *)

val angle: point -> float
(** [angle p] computes angle in degrees of point [p] between its radius and X axis.
    Value of the angle is between 0 inclusive and 360 exclusive. *)

val radius: point -> float
(** [radius p] computes distance of point [p] from zero point. *)
