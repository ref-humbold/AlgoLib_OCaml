(** Algorithms for points sorting *)

val sort_2d_by_x : Point2d.point2d list -> Point2d.point2d list
(** [sort_2d_by_x lst] sorts points [lst] with respect to their X coordinates. Sorting is guaranteed
    to be stable. *)

val sort_3d_by_x : Point3d.point3d list -> Point3d.point3d list
(** [sort_3d_by_x lst] sorts points [lst] with respect to their X coordinates. Sorting is guaranteed
    to be stable. *)

val sort_2d_by_y : Point2d.point2d list -> Point2d.point2d list
(** [sort_2d_by_y lst] sorts points [lst] with respect to their Y coordinates. Sorting is guaranteed
    to be stable. *)

val sort_3d_by_y : Point3d.point3d list -> Point3d.point3d list
(** [sort_3d_by_y lst] sorts points [lst] with respect to their Y coordinates. Sorting is guaranteed
    to be stable. *)

val sort_3d_by_z : Point3d.point3d list -> Point3d.point3d list
(** [sort_3d_by_z lst] sorts points [lst] with respect to their Z coordinates. Sorting is guaranteed
    to be stable. *)

val sort_2d_by_angle : Point2d.point2d list -> Point2d.point2d list
(** [sort_2d_by_angle lst] sorts points [lst] with respect to their polar coordinates. First sorts
    by angle, then by radius. *)
