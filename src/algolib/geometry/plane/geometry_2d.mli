(** Algorithms for basic geometrical computations *)

val sort_by_x : Point_2d.point2d list -> Point_2d.point2d list
(** [sort_by_x lst] sorts points [lst] with respect to their X coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_y : Point_2d.point2d list -> Point_2d.point2d list
(** [sort_by_y lst] sorts points [lst] with respect to their Y coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_angle : Point_2d.point2d list -> Point_2d.point2d list
(** [sort_by_angle lst] sorts points [lst] with respect to their polar coordinates. First sorts by
    angle, then by radius. *)
