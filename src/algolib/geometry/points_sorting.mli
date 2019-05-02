(** Points sorting algorithms. *)
open Point_2d

val angle_sort: point list -> point list
(** [angle_sort lst] sorts points [lst] on a plane with respect to their polar coordinates.
    First sorts by angle, then by radius. *)

val sort_by_x: point list -> point list
(** [lexi_sort lst] sorts points [lst] on a plane with respect to their coordinates.
    First sorts by X coordinate, then by Y coordinate. *)

val sort_by_y: point list -> point list
(** [lexi_sort lst] sorts points [lst] on a plane with respect to their coordinates.
    First sorts by Y coordinate, then by X coordinate. *)
