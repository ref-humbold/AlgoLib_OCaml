(** Graham's algorithm for convex hull on a plane *)

type point = float * float
(** The type of points in 2D *)

val find_convex_hull: point list -> point list
(** [find_convex_hull p] computes the convex hull of points [p] *)
