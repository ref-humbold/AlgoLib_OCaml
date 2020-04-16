(** Algorithms for points sorting *)

val sort_by_x : Point2d.point2d list -> Point2d.point2d list
(** [sort_by_x lst] sorts points [lst] with respect to their coordinates. First sorts by X
    coordinate, then by Y coordinate. *)

val sort_by_y : Point2d.point2d list -> Point2d.point2d list
(** [sort_by_y lst] sorts points [lst] with respect to their coordinates. First sorts by Y
    coordinate, then by X coordinate. *)

val sort_by_angle : Point2d.point2d list -> Point2d.point2d list
(** [angle_sort lst] sorts points [lst] with respect to their polar coordinates. First sorts by
    angle, then by radius. *)
