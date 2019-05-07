(** Graham's algorithm for convex hull on a plane. *)

(** The type of points in 2D. *)
type point = float * float

val find_convex_hull : point list -> point list
(** [find_convex_hull p] computes the convex hull of points [p]. *)
