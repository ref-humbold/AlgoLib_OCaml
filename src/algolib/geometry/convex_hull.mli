(** Graham's algorithm for convex hull on a plane. *)

val find_convex_hull : Point2d.point2d list -> Point2d.point2d list
(** [find_convex_hull p] computes the convex hull of points [p]. *)
