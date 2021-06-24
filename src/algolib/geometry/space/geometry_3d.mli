(** Algorithms for basic geometrical computations in 3D *)

val sort_by_x : Point_3d.point3d list -> Point_3d.point3d list
(** [sort_by_x lst] sorts points [lst] with respect to their X coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_y : Point_3d.point3d list -> Point_3d.point3d list
(** [sort_by_y lst] sorts points [lst] with respect to their Y coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_z : Point_3d.point3d list -> Point_3d.point3d list
(** [sort_by_z lst] sorts points [lst] with respect to their Z coordinates. Sorting is guaranteed to
    be stable. *)
