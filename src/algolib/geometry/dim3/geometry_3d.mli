val sort_by_x : Point_3d.t list -> Point_3d.t list
(** [sort_by_x ps] sorts points [ps] with respect to their X coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_y : Point_3d.t list -> Point_3d.t list
(** [sort_by_y ps] sorts points [ps] with respect to their Y coordinates. Sorting is guaranteed to
    be stable. *)

val sort_by_z : Point_3d.t list -> Point_3d.t list
(** [sort_by_z ps] sorts points [ps] with respect to their Z coordinates. Sorting is guaranteed to
    be stable. *)

val distance : Point_3d.t -> Point_3d.t -> float
(** [distance p1 p2] count the distance between points [p1] and [p2]. *)

val translate : Point_3d.t -> Vector_3d.t -> Point_3d.t
(** [translate p v] moves point [p] by vector [v]. *)

val reflect : Point_3d.t -> Point_3d.t -> Point_3d.t
(** [reflect p c] reflects point [p] in centre point [c]. *)
