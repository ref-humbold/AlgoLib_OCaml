val find_andrew_convex_hull : Point_2d.t list -> Point_2d.t list
(** [find_andrew_convex_hull ps] computes the convex hull of points [ps] using Andrew's monotone
    chain. *)

val find_graham_convex_hull : Point_2d.t list -> Point_2d.t list
(** [find_graham_convex_hull ps] computes the convex hull of points [ps] using Graham's scan. *)
