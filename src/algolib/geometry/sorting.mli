(** Points sorting algorithms. *)

type point = float * float

val angle_sort: point list -> point list
(** [angle_sort lst] sorts points [lst] on a plane with respect to their angles. *)

val lexi_sort: point list -> point list
(** [lexi_sort lst] sorts points [lst] on a plane with respect to their coordinates. *)
