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

val distance : Point_3d.point3d -> Point_3d.point3d -> float
(** [distance p1 p2] count the distance between points [p1] and [p2]. *)

val translate : Point_3d.point3d -> Vector_3d.vector3d -> Point_3d.point3d
(** [translate p v] moves point [p] by vector [v]. *)
