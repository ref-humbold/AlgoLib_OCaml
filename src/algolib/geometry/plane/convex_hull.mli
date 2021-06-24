(** Algorithm for convex hull in 2D (monotone chain) *)

val find_convex_hull : Point_2d.point2d list -> Point_2d.point2d list
(** [find_convex_hull p] computes the convex hull of points [p]. *)
