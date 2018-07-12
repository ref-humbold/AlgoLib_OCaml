(** GRAHAM'S ALGORITHM FOR CONVEX HULL ON A PLANE *)
type point = float * float

(** [find_convex_hull p] computes the convex hull of points [p]. *)
val find_convex_hull: point list -> point list
