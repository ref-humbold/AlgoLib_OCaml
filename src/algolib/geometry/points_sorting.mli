(** Points sorting algorithms. *)
open Point_2d

val angle_sort : point2d list -> point2d list
(** [angle_sort lst] sorts points [lst] with respect to their polar coordinates. First sorts by
    angle, then by radius. *)

val sort_by_x : point2d list -> point2d list
(** [sort_by_x lst] sorts points [lst] with respect to their coordinates. First sorts by X
    coordinate, then by Y coordinate. *)

val sort_by_y : point2d list -> point2d list
(** [sort_by_y lst] sorts points [lst] with respect to their coordinates. First sorts by Y
    coordinate, then by X coordinate. *)
